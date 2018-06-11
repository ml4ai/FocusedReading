package focusedreading.reinforcement_learning.environment

import focusedreading.Participant
import focusedreading.agents._
import org.sarsamora.environment.Environment
import org.sarsamora.actions.Action
import org.sarsamora.states.State
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.states.NormalizationParameters
import org.sarsamora.policies.Policy


/**
  * Created by enrique on 30/03/17.
  */
case class SimplePathEnvironment(participantA:Participant, participantB:Participant,
                            referencePath:Seq[Participant],
                            normalizationParameters:Option[NormalizationParameters])(implicit indexPath:LuceneIndexDir, sqliteFile:SQLiteFile) extends Environment {




  val agent = new PolicySearchAgent(participantA, participantB, None, Some(referencePath), normalizationParameters)

  override def possibleActions(): Seq[Action] = agent.possibleActions()

  override def execute(action: Action, persist: Boolean): Double = agent.executePolicy(action, persist)

  override def observeState: State = agent.observeState


  override def finishedEpisode:Boolean ={
    val ret = agent.hasFinished(participantA, participantB, agent.model, false)


    ret
  }

}
