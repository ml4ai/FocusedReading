package focusedreading.supervision.search.executable

import collection.JavaConversions._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.Configuration
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.{LibSVMClassifier, LinearKernel}
import org.clulab.learning.Datasets.mkTrainIndices
import org.clulab.learning.{RVFDataset, RVFDatum}
import org.clulab.utils.Serializer
import org.clulab.struct.Counter

object TrainSVMPolicyClassifier extends App with LazyLogging {

  // Syntactic sugarv...
  type SolutionsMap = Map[(String, String), Option[Seq[(FocusedReadingState, FocusedReadingAction, Double)]]]

  // Load the configuration parameters
  val trainingSolutionsPath = Configuration.ExpertOracle.trainingSolutionsPath
  val testingSolutionsPath = Configuration.ExpertOracle.testingSolutionsPath
  val classifierPath = Configuration.ExpertOracle.classifierPath
  val toBeIncluded = Configuration.ExpertOracle.activeFeatures.toSet

  // Desearalize the training data
  logger.info("Loading the training data")
  val rawDataSet = Serializer.load[SolutionsMap](trainingSolutionsPath)
  val rawTestingDataSet = Serializer.load[SolutionsMap](testingSolutionsPath)

  // Keep only those which are useful, and convert them into feature vectors
  val rawDataPoints = rawDataSet.values.collect { case Some(s) => s }.flatMap {
    case items =>
      items.map {
        case (state, action, _) => (state.toFeatures, action)
      }
  }

  val rawTestingDataPoints = rawTestingDataSet.values.collect { case Some(s) => s }.flatMap {
    case items =>
      items.map {
        case (state, action, _) => (state.toFeatures, action)
      }
  }

  // Get the feature names, sorted lexicographically
  val featureNames = rawDataPoints.head._1.keys.toSeq.sorted

  // Convert the data into Processors' RVFDatum instances and build the dataset
  logger.info("Marshalling the training data")
  val data = rawDataPoints map {case(m, l) =>
    (SVMPolicyClassifier.filterFeatures(m, toBeIncluded), l)} map SVMPolicyClassifier.toDatum

  val dataset = new RVFDataset[FocusedReadingAction, String]()

  data foreach { dataset += _}

  val testingData = rawTestingDataPoints map {case(m, l) =>
    (SVMPolicyClassifier.filterFeatures(m, toBeIncluded), l)} map SVMPolicyClassifier.toDatum


  // Instantiate a LibSVMClassifier with the default parameters
  logger.info("Training classifier")
  val classifier = new LibSVMClassifier[FocusedReadingAction, String](LinearKernel, C = 0.1, cacheSize = 200, probability = false)

  // Train it
  val x = dataset.labels map dataset.labelLexicon.get groupBy identity mapValues (_.size)
  val indices = mkTrainIndices(dataset.size, None)
  // TODO: Make this compute automatically
  val weights = Seq(ExploitEndpoints_ExploreManyQuery.asInstanceOf[FocusedReadingAction],
    ExploreEndpoints_ExploreManyQuery.asInstanceOf[FocusedReadingAction],
    ExploitEndpoints_ExploitQuery.asInstanceOf[FocusedReadingAction],
    ExploreEndpoints_ExploitQuery.asInstanceOf[FocusedReadingAction]).zip(Configuration.ExpertOracle.classWeights).map{case(k, v) => k -> v.toDouble}.toMap
  classifier.train(dataset, indices, Some(weights))

  // Test it on the held out data
  val predictions = testingData map classifier.classOf

  // Save the trained model to disk
  logger.info("Storing model")
  classifier.saveTo(classifierPath)


}