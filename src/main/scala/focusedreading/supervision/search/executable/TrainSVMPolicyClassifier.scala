package focusedreading.supervision.search.executable

import collection.JavaConversions._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import org.clulab.learning.{LibSVMClassifier, LinearKernel, RVFDataset, RVFDatum}
import org.clulab.utils.Serializer
import org.clulab.struct.Counter

object TrainSVMPolicyClassifier extends App with LazyLogging {

  // Syntactic sugarv...
  type SolutionsMap = Map[(String, String), Option[Seq[(FocusedReadingState, FocusedReadingAction, Double)]]]

  // Load the configuration parameters
  val config = ConfigFactory.load()

  val trainingConfig = config.getConfig("training")
  val supervisionConfig = config.getConfig("expertOracle")

  val solutionsPath = supervisionConfig.getString("solutionsPath")
  val classifierPath = supervisionConfig.getString("classifierPath")
  val toBeExcluded = supervisionConfig.getStringList("excludedFeatures").toSet

  // Desearalize the training data
  logger.info("Loading the training data")
  val dataSet = Serializer.load[SolutionsMap](solutionsPath)

  // Keep only those which are useful, and convert them into feature vectors
  val rawDataPoints = dataSet.values.collect { case Some(s) => s }.flatMap {
    case items =>
      items.map {
        case (state, action, _) => (state.toFeatures(), action)
      }
  }

  // Get the feature names, sorted lexicographically
  val featureNames = rawDataPoints.head._1.keys.toSeq.sorted

  // Convert the data into Processors' RVFDatum instances and build the dataset
  logger.info("Marshalling the training data")
  val data = rawDataPoints map {case(m, l) =>
    (SVMPolicyClassifier.removeFeatures(m, toBeExcluded), l)} map SVMPolicyClassifier.toDatum

  val dataset = new RVFDataset[FocusedReadingAction, String]()

  data foreach { dataset += _}


  // Instantiate a LibSVMClassifier with the default parameters
  logger.info("Training classifier")
  val classifier = new LibSVMClassifier[FocusedReadingAction, String](LinearKernel)

  // Train it
  classifier.train(dataset)

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
    * Helper method to filter out features not meant to be included in the training procedure
    * @param featuresMap Features' Map with the values
    * @param toBeRemoved Set with the feature names to be removed
    * @return A new map without the specified keys
    */
  def removeFeatures(featuresMap:Map[String, Double],
                     toBeRemoved:Set[String]):Map[String, Double] = featuresMap.filterNot{case (k, v) => toBeRemoved.contains(k)}

}