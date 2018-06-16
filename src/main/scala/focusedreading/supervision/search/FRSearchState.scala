package focusedreading.supervision.search

import focusedreading.agents.PolicySearchAgent
import focusedreading.entities.Participant
import focusedreading.supervision.ReferencePathSegment
import focusedreading.supervision.search.heuristics.DocumentSetIntersectionHeuristic

import scala.collection.mutable

case class FRSearchState(agent:PolicySearchAgent, groundTruth:Seq[ReferencePathSegment], depth:Int, maxIterations:Int){

  def remainingCost: Int = {
    val remainingSteps = groundTruth.collect{
      case ReferencePathSegment(a, b, papers) if !stepsDiscovered((a, b)) => papers
    }.flatten

    val set = collection.immutable.TreeSet.empty[String] ++ remainingSteps

    val n = DocumentSetIntersectionHeuristic.efficientUnionIntersectionSize(set, this.agent.papersRead)

    set.size - n
  }


  def this(agent:PolicySearchAgent, referenceState:FRSearchState) {
    this(agent, referenceState.groundTruth, referenceState.depth+1, referenceState.maxIterations)
    for(k <- referenceState.stepsDiscovered.keySet){
      stepsDiscovered(k) = referenceState.stepsDiscovered(k)
    }
  }

  private val steps = groundTruth map { case ReferencePathSegment(a, b, _) => (a, b)}
  val stepsDiscovered: mutable.Map[(String, String), Boolean] = new mutable.HashMap[(String, String), Boolean]() ++ (steps map { _ -> false })


  def finished:Boolean = stepsDiscovered.values.count(!_) == 0 // There shouldn't be any false element to be finished

  def cost:Double = {
    val failedState = depth > maxIterations

    if(failedState){
      Double.PositiveInfinity
    }
    else{
      agent.papersRead.size
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