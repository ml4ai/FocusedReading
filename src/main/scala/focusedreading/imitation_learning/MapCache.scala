package focusedreading.imitation_learning
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.SearchResult

import scala.collection.mutable

class MapCache extends SolutionsCache {

  private val cache = new mutable.HashMap[FocusedReadingState, Seq[SearchResult]]

  override def apply(state: FocusedReadingState): Seq[SearchResult] = cache(state)
  override def contains(state: FocusedReadingState): Boolean = cache.contains(state)
  override def get(state: FocusedReadingState): Option[Seq[SearchResult]] = cache.get(state)
  override def cache(state: FocusedReadingState, value: Seq[SearchResult]): Unit = cache += state -> value
}
