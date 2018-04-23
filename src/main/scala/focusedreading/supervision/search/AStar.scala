package focusedreading.supervision.search

import focusedreading.Participant
import focusedreading.supervision.search.heuristics.DocumentSetIntersectionHeuristic

import scala.collection.mutable

class AStar(initialState:FRSearchState, maxCost:Double = Double.PositiveInfinity)
  extends UniformCostSearch(initialState, maxCost) {

  // Instantiate the heuristic object
  private val steps = initialState.groundTruth.map( gt => (Participant.get("", gt._1), Participant.get("", gt._2)))
  private val querier = initialState.agent.redisLuceneQuerier
  private val heuristic = new DocumentSetIntersectionHeuristic(steps, querier)

  override val queue = new mutable.PriorityQueue[Node]()(Ordering.by[Node, Double](n => n.pathCost + heuristic.estimateCost(n.state)))
}
