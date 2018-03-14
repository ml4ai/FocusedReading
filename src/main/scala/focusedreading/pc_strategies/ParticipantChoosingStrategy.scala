package focusedreading.pc_strategies

import focusedreading.Participant
import focusedreading.models._
import focusedreading.pc_strategies.ParticipantChoosingStrategy.Color
import focusedreading.reinforcement_learning.actions._
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.states.State

import scala.collection.mutable

/**
  * Created by enrique on 19/02/17.
  */
trait ParticipantChoosingStrategy {
  def choseEndPoints(source:Participant,
                     destination:Participant,
                     colors:mutable.Map[Participant, Color],
                     //previouslyChosen:Set[(Participant, Participant)],
                     model:SearchModel):Seq[Participant]


  //// LEGACY
  /**
    * Checks wether there's a change from the latest endpoints
    */
  def differentEndpoints(a:(Participant, Participant), previouslyChosen:Set[(Participant, Participant)]) = !previouslyChosen.contains(a)

  /**
    * Picks two enpoints from the stacks
    * @param sA
    * @param sB
    * @return
    */
  def pickEndpoints(sA:mutable.Stack[Participant], sB:mutable.Stack[Participant]):(Participant, Participant) = {

    val (left, right) = if(sA.size <= sB.size) (sA, sB) else (sB, sA)
    val a = left.pop()
    var b = right.pop()

    var stop = false
    while(!stop){
      if(right.nonEmpty) {
        b = right.pop()
        stop = a != b
      }
      else
        stop = true
    }

    (a, b)
  }
  /////////////
}

object ParticipantChoosingStrategy{
  object Color extends Enumeration{
    val White, Gray, Black = Value
  }

  type Color = Color.Value
}