package focusedreading.supervision.search

import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState

// State, action, Cost, estimated remaining cost, Actual Remaining cost
case class SearchResult(
  state:FocusedReadingState,
  action:FocusedReadingAction,
  cost:Double, estimatedRemaining:Option[Int],
  actualRemaining:Option[Int]
)
