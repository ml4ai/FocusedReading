package focusedreading.supervision.search.executable

import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import org.clulab.learning.RVFDatum
import org.clulab.struct.Counter

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