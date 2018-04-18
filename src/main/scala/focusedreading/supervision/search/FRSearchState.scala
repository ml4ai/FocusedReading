package focusedreading.supervision.search

import focusedreading.Participant
import focusedreading.agents.PolicySearchAgent
import focusedreading.supervision.search.FRSearchState.GoldDatum

import scala.collection.mutable

case class FRSearchState(agent:PolicySearchAgent, groundTruth:GoldDatum, depth:Int, maxIterations:Int){



  def this(agent:PolicySearchAgent, referenceState:FRSearchState) {
    this(agent, referenceState.groundTruth, referenceState.depth+1, referenceState.maxIterations)
    for(k <- referenceState.stepsDiscovered.keySet){
      stepsDiscovered(k) = referenceState.stepsDiscovered(k)
    }
  }

  private val steps = groundTruth map { case(a, b, _) => (a, b)}
  val stepsDiscovered: mutable.Map[(String, String), Boolean] = new mutable.HashMap[(String, String), Boolean]() ++ (steps map { _ -> false })


  def finished:Boolean = stepsDiscovered.values.count(!_) == 0 // There shouldn't be any false element to be finished

  def cost:Double = {
    val failedState = depth > maxIterations

    if(failedState){
      Double.PositiveInfinity
    }
    else{
      agent.papersRead.size //  TODO: Keep an eye on this, maybe change it for a better cost function, consider the action length
    }
  }

  def updateState():Unit = {
    val pending = steps dropWhile stepsDiscovered

    pending foreach {
      k =>
        val path = agent.model.shortestPath(Participant.get("", k._1), Participant.get("", k._2))
        path match {
          case Some(_) => stepsDiscovered(k) = true
          case None => Unit
        }

    }
  }
}

object FRSearchState {
  type GoldDatum = Seq[(String, String, Seq[String])]
}