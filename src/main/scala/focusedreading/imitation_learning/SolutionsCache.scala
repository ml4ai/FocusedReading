package focusedreading.imitation_learning

import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.executable.DoSearch.Result

trait SolutionsCache {

  def apply(state:FocusedReadingState): Seq[Result]
  def contains(state:FocusedReadingState):Boolean
  def get(state:FocusedReadingState):Option[Seq[Result]]
  def cache(state:FocusedReadingState, value:Seq[Result]): Unit

}
