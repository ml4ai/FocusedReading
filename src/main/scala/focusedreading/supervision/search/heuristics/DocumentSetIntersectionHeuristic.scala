package focusedreading.supervision.search.heuristics

import com.typesafe.scalalogging.LazyLogging
import focusedreading.Participant
import focusedreading.ie.IEStrategy
import focusedreading.ir.LuceneQueries
import focusedreading.supervision.search.FRSearchState

/**
  * Implements a heuristic for A*
  * @param links Ground-thurth links
  * @param querier LuceneQueries instance to compute the set intersections
  */
class DocumentSetIntersectionHeuristic(links:Seq[(Participant, Participant)], querier:LuceneQueries) extends LazyLogging{

  // Sets of documents containing both entities simultaneously
  val documentSets: Map[(String, String), Set[String]] = links.map{
    case (a, b) =>
      val queryRes = querier.binaryConjunctionQuery(a, b, None).map(_._1).toSet
      if(queryRes.isEmpty)
        logger.info(s"($a, $b) returned an empty set")
      (a.id, b.id) -> queryRes
  }.toMap

  /**
    * Returns an estimated cost to be used as a heuristic by a search algorithm
    * @param state Search state to figure out which links are still left to find
    * @return The estimated cost to completion
    */
  def estimateCost(state:FRSearchState):Double = {
    // First, given the current state, how many links are left to find
    val stepsToFind = state.stepsDiscovered.keys.filterNot(state.stepsDiscovered)
    val setsToConsider = stepsToFind.map(documentSets)

    // Compute the union of all the document sets that are considered
    val union = setsToConsider.fold(Set.empty[String])((a, b) => a union b)

    // Return the size of the union
    union.size.toDouble
  }

}
