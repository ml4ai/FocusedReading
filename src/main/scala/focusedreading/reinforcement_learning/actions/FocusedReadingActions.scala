package focusedreading.reinforcement_learning.actions

/**
  * Created by enrique on 17/04/17.
  */

import org.json4s.DefaultFormats

import collection.mutable
import org.json4s.JsonAST.{JArray, JObject, JValue}
import org.sarsamora.actions.Action
import org.sarsamora.states.{State, StateParser}
import org.sarsamora.value_functions.ActionValueLoader

sealed class FocusedReadingAction() extends Action

object FocusedReadingAction {
  def apply(str:String): FocusedReadingAction = {
    str match {
      case "ExploitEndpoints_ExploreManyQuery" => ExploitEndpoints_ExploreManyQuery
      case "ExploitEndpoints_ExploreFewQuery" => ExploitEndpoints_ExploreFewQuery
      case "ExploitEndpoints_ExploitQuery" => ExploitEndpoints_ExploitQuery
      case "ExploreEndpoints_ExploreManyQuery" => ExploreEndpoints_ExploreManyQuery
      case "ExploreEndpoints_ExploreFewQuery" => ExploreEndpoints_ExploreFewQuery
      case "ExploreEndpoints_ExploitQuery" => ExploreEndpoints_ExploitQuery
      case _ => throw new Exception("Shouln't fall here. Check!!")
    }
  }

  // All the possible actions
  // TODO: make a way to handle this as an enumeration
  val allActions = Seq(
    ExploitEndpoints_ExploreManyQuery,
    ExploitEndpoints_ExploreFewQuery,
    ExploitEndpoints_ExploitQuery,
    ExploreEndpoints_ExploreManyQuery,
    ExploreEndpoints_ExploreFewQuery,
    ExploreEndpoints_ExploitQuery
  )
}

case object ExploitEndpoints_ExploreManyQuery extends FocusedReadingAction
case object ExploitEndpoints_ExploreFewQuery extends FocusedReadingAction
case object ExploitEndpoints_ExploitQuery extends FocusedReadingAction
case object ExploreEndpoints_ExploreManyQuery extends FocusedReadingAction
case object ExploreEndpoints_ExploreFewQuery extends FocusedReadingAction
case object ExploreEndpoints_ExploitQuery extends FocusedReadingAction