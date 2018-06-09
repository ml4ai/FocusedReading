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
}

case object ExploitEndpoints_ExploreManyQuery extends FocusedReadingAction
case object ExploitEndpoints_ExploreFewQuery extends FocusedReadingAction
case object ExploitEndpoints_ExploitQuery extends FocusedReadingAction
case object ExploreEndpoints_ExploreManyQuery extends FocusedReadingAction
case object ExploreEndpoints_ExploreFewQuery extends FocusedReadingAction
case object ExploreEndpoints_ExploitQuery extends FocusedReadingAction


class FocusedReadingActionValues extends ActionValueLoader with StateParser {
  implicit lazy val formats = DefaultFormats


  private def extractCoefficients(ast:JValue, name:FocusedReadingAction):Option[(Action, mutable.HashMap[String, Double])] = {
    ast \ name.toString match {
      case JObject(obj) =>
        val coefficients = new mutable.HashMap[String, Double]()

        for((k, v) <- obj){
          coefficients += (k -> v.extract[Double])
        }

        Some(name -> coefficients)

      case _ =>
        None
    }
  }

  override def loadActionValues(ast:JObject) = {

    val coefficients = ast \ "coefficients"
    val valsExploitEpExploreManyQ = extractCoefficients(coefficients, ExploitEndpoints_ExploreManyQuery)
    val valsExploitEpExploreFewQ = extractCoefficients(coefficients, ExploitEndpoints_ExploreFewQuery)
    val valsExploitEpExploitQ = extractCoefficients(coefficients, ExploitEndpoints_ExploitQuery)
    val valsExploreEpExploreFewQ = extractCoefficients(coefficients, ExploreEndpoints_ExploreFewQuery)
    val valsExploreEpExploreManyQ = extractCoefficients(coefficients, ExploreEndpoints_ExploreManyQuery)
    val valsExploreEpExploitQ = extractCoefficients(coefficients, ExploreEndpoints_ExploitQuery)

    val coefficientsMap = Seq[Option[(Action, mutable.HashMap[String, Double])]](valsExploitEpExploreManyQ, valsExploitEpExploreFewQ, valsExploitEpExploitQ, valsExploreEpExploreFewQ, valsExploreEpExploreManyQ, valsExploreEpExploitQ).collect{
      case Some((name, coeff)) => name -> coeff
    }.toMap

    coefficientsMap
  }

  override val stateParser = this

  override def fromString(description: String):State = {
    throw new Exception("Not implemented")
  }
}