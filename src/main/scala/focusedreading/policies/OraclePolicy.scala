package focusedreading.policies

import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.states.State

class OraclePolicy extends Policy {
  override def selectAction(s: State, possibleActions: Seq[Action]): Action = ???

  override def save(path: String): Unit = Unit
}
