package focusedreading.imitation_learning

import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.SearchResult

trait SolutionsCache {

  def apply(state:FocusedReadingState): Seq[SearchResult]
  def contains(state:FocusedReadingState):Boolean
  def get(state:FocusedReadingState):Option[Seq[SearchResult]]
  def cache(state:FocusedReadingState, value:Seq[SearchResult]): Unit

}
