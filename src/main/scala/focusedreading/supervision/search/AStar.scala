package focusedreading.supervision.search

import focusedreading.entities.Participant
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.supervision.search.heuristics.DocumentSetIntersectionHeuristic

import scala.collection.mutable

class AStar(initialState:FRSearchState, maxCost:Double = Double.PositiveInfinity)
  extends UniformCostSearch(initialState, maxCost) {

  // Instantiate the heuristic object
  private val steps = initialState.groundTruth.map( gt => (Participant.get("", gt.source), Participant.get("", gt.destination)))
  private val querier = initialState.agent.redisLuceneQuerier
  private val heuristic = new DocumentSetIntersectionHeuristic(steps, querier)

  override val queue = new mutable.PriorityQueue[Node]()(Ordering.by[Node, Double](n => n.pathCost + heuristic.estimateCost(n.state)))

  override def createNode(state: FRSearchState, node: Option[Node], action: Option[FocusedReadingAction]): Node = {
    Node(state, /*state.cost.toInt, heuristic.estimateCost(state).toInt, state.remainingCost,*/ action, node, node match { case Some(n) => n.depth +1; case None => 0})
  }

  override def estimateRemaining(state: FRSearchState): Int = {
    heuristic.estimateCost(state).toInt
  }
}
