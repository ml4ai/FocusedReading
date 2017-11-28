package org.clulab.reach.focusedreading.reinforcement_learning.states

/**
  * Created by enrique on 03/31/17.
  * Last updated by enrique on 11/17/17
  *
  * Contains the state representation data structure and related code for Focused Reading
  */

import org.sarsamora.states.State

object RankBin extends Enumeration {
  val First, Upper, Mid, Bottom = Value

  def toFeatures(b:RankBin.Value, prefix:String):Map[String, Double] = {
    // Get rid of one bin to avoid mulitcolinearity in the design matrix
    val values = RankBin.values.toSeq//.dropRight(1)

    values.map{
      v =>
        val is = if(v == b) 1.0 else 0.0
        (s"$prefix-${v.toString}" -> is)
    }.toMap
  }
}

/**
  * State representation of the FR search process
  * @param paRank Participant A (PA) rank in it's component
  * @param pbRank Participant B (PB) rank in it's component
  * @param iteration Iteration number of the process
  * @param paQueryLogCount How many time has PA being used to anchor the IR search
  * @param pbQueryLogCount How many time has PB being used to anchor the IR search
  * @param sameComponent Whether PA and PB in the same connected component
  * @param paIterationIntroduction Iteration # in which PA was introduced to the KB graph
  * @param pbIterationIntroduction Iteration # in which PB was introduced to the KB graph
  * @param paUngrounded Whether PA has a grounding ID (not a UAZ id generated autimatically by REACH)
  * @param pbUngrounded Whether PB has a grounding ID (not a UAZ id generated autimatically by REACH)
  * @param exploreIRScore IR aggregated score of the exploration IR query
  * @param exploitIRScore IR aggregated score of the exploritation IR query
  */
case class FocusedReadingState(paRank:Double,
                               pbRank:Double,
                               iteration:Int,
                               paQueryLogCount:Int,
                               pbQueryLogCount:Int,
                               sameComponent:Boolean,
                               paIterationIntroduction:Int,
                               pbIterationIntroduction:Int,
                               paUngrounded:Boolean,
                               pbUngrounded:Boolean,
                               exploreIRScore:Double,
                               exploitIRScore:Double
                              ) extends State{

  override def hashCode(): Int = {
    // TODO: Automatically calculate this using reflection
    s"$paRank-$pbRank-$iteration-$paQueryLogCount-$pbQueryLogCount-$sameComponent-$paIterationIntroduction-$pbIterationIntroduction--$paUngrounded-$pbUngrounded".hashCode
  }

  override def equals(obj: scala.Any): Boolean = {
    // TODO: Automatically calculate this using reflection
    if(obj.getClass == this.getClass){
      val that = obj.asInstanceOf[FocusedReadingState]
      if(paRank == that.paRank
        && pbRank == that.pbRank
        && iteration == that.iteration
        && paQueryLogCount == that.paQueryLogCount
        && pbQueryLogCount == that.pbQueryLogCount
        && sameComponent == that.sameComponent
        && paIterationIntroduction == that.paIterationIntroduction
        && pbIterationIntroduction == that.pbIterationIntroduction
        && exploreIRScore == that.exploitIRScore
        && exploitIRScore == that.exploitIRScore)
        true
      else
        false
    }
    else{
      false
    }
  }

  /**
    * Convert the state representation to feature values used by a learning component
    * @return Map of feature names -> feature values
    */
  override def toFeatures():Map[String, Double] = {
    Map(
      "iteration" -> iteration.toDouble,
      "paQueryLogCount" -> paQueryLogCount.toDouble,
      "pbQueryLogCount" -> pbQueryLogCount.toDouble,
      "sameComponent" ->  (sameComponent match{ case true => 1.0; case false => 0.0 }),
      "paIterationIntroduction" -> paIterationIntroduction.toDouble,
      "pbIterationIntroduction" -> pbIterationIntroduction.toDouble,
      "paRank" -> paRank,
      "pbRank" -> pbRank
      //"exploreIRScore" -> exploreIRScore,
      //"exploitIRScore" -> exploitIRScore
      //"paUngrounded" -> (paUngrounded match { case true => 1.0; case false => 0.0}),
      //"pbUngrounded" -> (pbUngrounded match { case true => 1.0; case false => 0.0})
    )  //++ RankBin.toFeatures(paRank, "paRank") ++ RankBin.toFeatures(pbRank, "pbRank")
  }
}
