package focusedreading.supervision.search.heuristics

import com.typesafe.scalalogging.LazyLogging
import focusedreading.Participant
import focusedreading.ie.IEStrategy
import focusedreading.ir.LuceneQueries
import focusedreading.supervision.search.FRSearchState

import scala.collection.SortedSet
import scala.collection.immutable.TreeSet

/**
  * Implements a heuristic for A*
  * @param links Ground-thurth links
  * @param querier LuceneQueries instance to compute the set intersections
  */
class DocumentSetIntersectionHeuristic(links:Seq[(Participant, Participant)], querier:LuceneQueries) extends LazyLogging{

  // Sets of documents containing both entities simultaneously
  val documentSets: Map[(String, String), SortedSet[String]] = links.map{
    case (a, b) =>
      val queryRes = querier.binaryConjunctionQuery(a, b, None).map(_._1)//.toSet
      if(queryRes.isEmpty)
        logger.info(s"($a, $b) returned an empty set")
      (a.id, b.id) -> (TreeSet.empty[String] ++ queryRes)
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
    val union = setsToConsider.fold(TreeSet.empty[String])((a, b) => a union b)

    // Subtract the papers that have already been read by the system
    efficientUnionIntersectionSize(union, state.agent.papersRead)

    // Return the size of the union
    //union.size.toDouble
  }

  private def efficientUnionIntersectionSize(a:SortedSet[String], b:SortedSet[String]):Int = {


    if(a.isEmpty || b.isEmpty)
      return 0

    val (shorter, longer) = if(a.size <= b.size) (a.iterator, b.iterator) else (b.iterator, a.iterator)

    var size = -1

    var x:String = null
    var y:String = null

    while(shorter.hasNext && longer.hasNext){
      if(x == y){
        size += 1
        x = if(shorter.hasNext) shorter.next() else null
        y = if(longer.hasNext) longer.next() else null
      }
      else if(x < y){
        x = if(shorter.hasNext) shorter.next() else null
      }
      else{ //if(y > x)
        y = if(longer.hasNext) longer.next() else null
      }
    }

    // Left over elements
    if(x != null && y != null){
      if(x == y)
        size += 1
    }


    size
  }

}
