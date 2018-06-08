package focusedreading.imitation_learning
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.executable.DoSearch.Result

import scala.collection.mutable

class MapCache extends SolutionsCache {

  private val cache = new mutable.HashMap[FocusedReadingState, Seq[Result]]

  override def apply(state: FocusedReadingState): Seq[Result] = cache(state)
  override def contains(state: FocusedReadingState): Boolean = cache.contains(state)
  override def get(state: FocusedReadingState): Option[Seq[Result]] = cache.get(state)
  override def cache(state: FocusedReadingState, value: Seq[Result]): Unit = cache += state -> value
}
