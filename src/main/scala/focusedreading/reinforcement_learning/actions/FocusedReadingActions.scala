package org.clulab.reach.focusedreading.reinforcement_learning.actions

/**
  * Created by enrique on 17/04/17.
  */

import org.json4s.DefaultFormats

import collection.mutable
import org.json4s.JsonAST.{JArray, JObject, JValue}
import org.sarsamora.actions.Action
import org.sarsamora.policies.ActionValueLoader

sealed class FocusedReadingAction() extends Action

case class ExploreQuery() extends FocusedReadingAction

case class ExploitQuery() extends FocusedReadingAction

case class ExploreEndpoints() extends FocusedReadingAction

case class ExploitEndpoints() extends FocusedReadingAction


class FocusedReadingActionValues extends ActionValueLoader {
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
    val valsExploreQuery = extractCoefficients(coefficients, ExploreQuery())
    val valsExploitQuery = extractCoefficients(coefficients, ExploitQuery())
    val valsExploreEndpoints = extractCoefficients(coefficients, ExploreEndpoints())
    val valsExploitEndpoints = extractCoefficients(coefficients, ExploitEndpoints())

    val coefficientsMap = Seq[Option[(Action, mutable.HashMap[String, Double])]](valsExploreQuery, valsExploitQuery, valsExploreEndpoints, valsExploitEndpoints).collect{
      case Some((name, coeff)) => name -> coeff
    }.toMap

    coefficientsMap
  }
}