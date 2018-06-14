package focusedreading.supervision.search.executable

import collection.JavaConversions._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
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
  val config = ConfigFactory.load()

  val trainingConfig = config.getConfig("training")
  val supervisionConfig = config.getConfig("expertOracle")

  val trainingSolutionsPath = supervisionConfig.getString("trainingSolutionsPath")
  val testingSolutionsPath = supervisionConfig.getString("solutionsPath")
  val classifierPath = supervisionConfig.getString("classifierPath")
  val toBeIncluded = supervisionConfig.getStringList("includedFeatures").toSet

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
    ExploreEndpoints_ExploitQuery.asInstanceOf[FocusedReadingAction]).zip(supervisionConfig.getDoubleList("classWeights")).map{case(k, v) => k -> v.toDouble}.toMap
  classifier.train(dataset, indices, Some(weights))

  // Test it on the held out data
  val predictions = testingData map classifier.classOf

  // Save the trained model to disk
  logger.info("Storing model")
  classifier.saveTo(classifierPath)


}


object SVMPolicyClassifier {

  /**
    * Creates an RVFDatum instance out of a dictionary features
    * @param rawDatum
    * @return
    */
  def toDatum(rawDatum:(Map[String, Double], FocusedReadingAction)):RVFDatum[FocusedReadingAction, String] = {
    // Counter is the backend used for the feature values. It's awkward to use it this way, as it was meant to be used
    // as a bag-of-words, but it is what it is.
    val c = new Counter[String]
    // Unbox the parameter
    val (features, label) = rawDatum
    // assign the feature values in the counter
    for((k, v) <- features) c.incrementCount(k, v)
    // Wrap them up with an RVFDatum instance!
    new RVFDatum[FocusedReadingAction, String](label, c)

  }

  /**
    * Helper method to filter in features meant to be included in the training procedure
    * @param featuresMap Features' Map with the values
    * @param toBeKept Set with the feature names to be removed
    * @return A new map without the specified keys
    */
  def filterFeatures(featuresMap:Map[String, Double],
                     toBeKept:Set[String]):Map[String, Double] = featuresMap.filter{case (k, v) => toBeKept.contains(k)}

}