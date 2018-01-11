package focusedreading.reinforcement_learning.environment

import focusedreading.Participant
import focusedreading.agents.{PolicySearchAgent, RedisSQLiteSearchAgent}
import org.sarsamora.environment.Environment
import org.sarsamora.actions.Action
import org.sarsamora.policies.DummyPolicy
import org.sarsamora.states.State
import focusedreading.agents.FocusedReadingStage
import focusedreading.reinforcement_learning.actions._


/**
  * Created by enrique on 30/03/17.
  */
class SimplePathEnvironment(participantA:Participant, participantB:Participant, referencePath:Seq[Participant]) extends Environment {

  val agent = new PolicySearchAgent(participantA, participantB, DummyPolicy(), Some(referencePath))

  override def possibleActions(): Seq[Action] = agent.possibleActions()

  override def executePolicy(action: Action, persist: Boolean): Double = agent.executePolicy(action, persist)

  override def observeState: State = agent.observeState

  override def observeStates: Seq[State] = {
    (agent.stage: @unchecked) match {
      case FocusedReadingStage.Query =>
        val state = agent.observeState
        // Repeat the state the number of different action sizes available to zip it with the possible actions
        // This is done because the state is dependent of the action to be chosen, feels incorrect TODO: Double check this is kosher
        Seq.fill(PolicySearchAgent.getActiveQueryActions.size)(state)
      case FocusedReadingStage.EndPoints =>
        // TODO: Is it valid if the state is a function of the action? Verify this is valid
        val exploreState = agent.observeExploreState(participantA, participantB, agent.triedPairs.toSet, agent.model)._2
        val exploitState = agent.observeExploitState(participantA, participantB, agent.triedPairs.toSet, agent.model)._2

        val actions = PolicySearchAgent.getActiveEndpointActions.toSeq

        actions.map{
          case _:ExploreEndpoints =>
            exploreState
          case _:ExploitEndpoints =>
            exploitState
        }
    }
  }

  override def finishedEpisode:Boolean = agent.stage == FocusedReadingStage.EndPoints && agent.hasFinished(participantA, participantB, agent.model, false)

}
