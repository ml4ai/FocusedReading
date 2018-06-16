package focusedreading.reinforcement_learning.actions

import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JObject, JValue}
import org.sarsamora.actions.Action
import org.sarsamora.states.{State, StateParser}
import org.sarsamora.value_functions.ActionValueLoader

import scala.collection.mutable

class FocusedReadingActionValues extends ActionValueLoader with StateParser {
  private implicit lazy val formats = DefaultFormats


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

  override def loadActionValues(ast:JObject): Map[Action, mutable.HashMap[String, Double]] = {

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

  override val stateParser: FocusedReadingActionValues = this

  override def fromString(description: String):State = {
    throw new Exception("Not implemented")
  }
}
