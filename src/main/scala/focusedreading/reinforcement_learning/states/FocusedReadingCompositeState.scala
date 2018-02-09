package focusedreading.reinforcement_learning.states

import org.sarsamora.states.State

/**
  * This class combines two different FR states into a single state with all the elements and feature names
  * prefixed by the strategy name
  */
case class FocusedReadingCompositeState(exploreState:FocusedReadingState, exploitState:FocusedReadingState) extends State{
  override def toFeatures:Map[String, Double] = {
     FocusedReadingCompositeState.featurePrefix zip Seq(exploreState, exploitState) flatMap {
      case(prefix, state) =>
        val features = state.toFeatures()
        features map {
          case(name, value) => s"${prefix}_$name" -> value
        }
    } toMap
  }
}

object FocusedReadingCompositeState {
  val featurePrefix: Seq[String] = Seq("explore", "exploit")

  def featureNames:Set[String] = featurePrefix flatMap  {
    prefix =>
      FocusedReadingState.featureNames map {
        name => s"${prefix}_$name"
      }
  } toSet
}