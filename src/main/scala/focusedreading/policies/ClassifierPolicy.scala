package focusedreading.policies

import collection.JavaConversions._
import com.typesafe.config.ConfigFactory
import focusedreading.reinforcement_learning.actions.{ExploitEndpoints_ExploitQuery, FocusedReadingAction}
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.executable.SVMPolicyClassifier
import focusedreading.supervision.search.LibSVMClassifier
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.states.State
import org.clulab.utils.Serializer

class ClassifierPolicy(classifier:LibSVMClassifier[FocusedReadingAction, String]) extends Policy {

  def this(classifierPath:String) = {
    this(LibSVMClassifier.loadFrom[FocusedReadingAction, String](classifierPath))
  }

  // Load the configuration parameters
  private val config = ConfigFactory.load()

  private val supervisionConfig = config.getConfig("expertOracle")

  private val toBeExcluded = supervisionConfig.getStringList("includedFeatures").toSet

  override def selectAction(s: State, possibleActions: Seq[Action]): Action = {
    // Create a datum out of the state
    val features = SVMPolicyClassifier.filterFeatures(s.asInstanceOf[FocusedReadingState].toFeatures(false), toBeExcluded)
    // Ugly to add a dummy label, but hey
    val datum = SVMPolicyClassifier.toDatum((features, ExploitEndpoints_ExploitQuery()))
    // Classify it
    val predictedAction = classifier.classOf(datum)

    // Return the action after a sanity check
    if(possibleActions contains predictedAction)
      predictedAction
    else{
      throw new Exception("The predicted action is not in the class")
    }
  }

  /***
    * Don't need to implement this
    * @param path
    */
  override def save(path: String): Unit = {
    classifier.saveTo(path)
  }
}
