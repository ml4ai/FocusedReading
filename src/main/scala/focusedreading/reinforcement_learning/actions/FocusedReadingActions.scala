package org.clulab.reach.focusedreading.reinforcement_learning.actions

/**
  * Created by enrique on 17/04/17.
  */

import org.json4s.DefaultFormats

import collection.mutable
import org.json4s.JsonAST.JObject
import org.sarsamora.actions.Action
import org.sarsamora.policies.ActionValueLoader

sealed class FocusedReadingAction() extends Action

case class ExploreQuery() extends FocusedReadingAction

case class ExploitQuery() extends FocusedReadingAction

case class ExploreEndpoints() extends FocusedReadingAction

case class ExploitEndpoints() extends FocusedReadingAction


class FocusedReadingActionValues extends ActionValueLoader {
  implicit lazy val formats = DefaultFormats

  override def loadActionValues(ast:JObject) = {
    val valsExploreQuery = (ast \ "coefficientsExploreQuery").asInstanceOf[JObject].obj
    val valsExploitQuery = (ast \ "coefficientsExploitQuery").asInstanceOf[JObject].obj
    val valsExploreEndpoints = (ast \ "coefficientsExploreEndpoints").asInstanceOf[JObject].obj
    val valsExploitEndpoints = (ast \ "coefficientsExploitEndpoints").asInstanceOf[JObject].obj


    // Make a map out of the coefficients
    val coefficientsExploreQuery = new mutable.HashMap[String, Double]
    for ((k, v) <- valsExploreQuery) {
      coefficientsExploreQuery += (k -> v.extract[Double])
    }

    val coefficientsExploreEndpoints = new mutable.HashMap[String, Double]
    for ((k, v) <- valsExploreEndpoints) {
      coefficientsExploreEndpoints += (k -> v.extract[Double])
    }

    val coefficientsExploitQuery = new mutable.HashMap[String, Double]
    for ((k, v) <- valsExploitQuery) {
      coefficientsExploitQuery += (k -> v.extract[Double])
    }

    val coefficientsExploitEndpoints = new mutable.HashMap[String, Double]
    for ((k, v) <- valsExploitEndpoints) {
      coefficientsExploitEndpoints += (k -> v.extract[Double])
    }

    val coefficientsMap = Map[Action, mutable.HashMap[String, Double]](
      ExploreQuery().asInstanceOf[Action] -> coefficientsExploreQuery,
      ExploitQuery().asInstanceOf[Action] -> coefficientsExploitQuery,
      ExploreEndpoints() -> coefficientsExploreEndpoints,
      ExploitEndpoints() -> coefficientsExploitEndpoints
    )

    coefficientsMap
  }
}