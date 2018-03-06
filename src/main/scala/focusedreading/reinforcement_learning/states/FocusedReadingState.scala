package focusedreading.reinforcement_learning.states

/**
  * Created by enrique on 03/31/17.
  * Last updated by enrique on 11/17/17
  *
  * Contains the state representation data structure and related code for Focused Reading
  */

import focusedreading.agents.FocusedReadingStage

import collection.mutable
import org.sarsamora.states.State

object RankBin extends Enumeration {
  val First, Upper, Mid, Bottom = Value

  def toFeatures(b:RankBin.Value, prefix:String):Map[String, Double] = {
    // Get rid of one bin to avoid mulitcolinearity in the design matrix
    val values = RankBin.values.toSeq//.dropRight(1)

    values.map{
      v =>
        val is = if(v == b) 1.0 else 0.0
        s"$prefix-${v.toString}" -> is
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
  * @param exploreFewIRScores IR scores of the exploration IR query
  * @param exploreManyIRScores IR scores of the exploration IR query
  * @param exploitIRScores IR scores of the exploritation IR query
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
                               exploreFewIRScores:Seq[Float],
                               exploreManyIRScores:Seq[Float],
                               exploitIRScores:Seq[Float],
                               unchangedIterations:Int,
                               normalizationParameters: Option[NormalizationParameters]
                              ) extends State{

  // Aggregate the scores
  val exploreFew: (Float, Float, Float) = aggregateIRScores(exploreFewIRScores)
  val exploreMany: (Float, Float, Float) = aggregateIRScores(exploreManyIRScores)
  val exploit: (Float, Float, Float) = aggregateIRScores(exploitIRScores)

  private def aggregateIRScores(scores:Seq[Float]):(Float, Float, Float) = {
    if(scores.isEmpty)
      (0.0f, 0.0f, 0.0f)
    else{
      (scores.min,
        scores.max,
        scores.sum / scores.size)
    }
  }

  /**
    * Convert the state representation to feature values used by a learning component
    * @return Map of feature names -> feature values
    */
  override def toFeatures():Map[String, Double] = {


    val featureValues = Map[String, Double](
      "iteration" -> iteration.toDouble,
      "paQueryLogCount" -> paQueryLogCount.toDouble,
      "pbQueryLogCount" -> pbQueryLogCount.toDouble,
      "sameComponent" ->  (sameComponent match{ case true => 1.0; case false => 0.0 }),
      "paIterationIntroduction" -> paIterationIntroduction.toDouble,
      "pbIterationIntroduction" -> pbIterationIntroduction.toDouble,
      "paRank" -> paRank,
      "pbRank" -> pbRank,
      "exploreFewIRScore_min" -> exploreFew._1,
      "exploreFewIRScore_max" -> exploreFew._2,
      "exploreFewIRScore_mean" -> exploreFew._3,
      "exploreManyIRScore_min" -> exploreMany._1,
      "exploreManyIRScore_max" -> exploreMany._2,
      "exploreManyIRScore_mean" -> exploreMany._3,
      "exploitIRScore_min" -> exploit._1,
      "exploitIRScore_max" -> exploit._2,
      "exploitIRScore_mean" -> exploit._3,
      "unchangedIterations" -> unchangedIterations,
      "paUngrounded" -> (paUngrounded match { case true => 1.0; case false => 0.0}),
      "pbUngrounded" -> (pbUngrounded match { case true => 1.0; case false => 0.0})
    )  //++ RankBin.toFeatures(paRank, "paRank") ++ RankBin.toFeatures(pbRank, "pbRank")


    // Keep trak of the feature values
    FocusedReadingState.recordObsevation(featureValues)

    // Normalize if requested
    val retVal = this.normalizationParameters match {
      case Some(parameters) => parameters.normalize(featureValues)
      case None => featureValues
    }

    retVal
  }
}

/**
  * Companion object to the FocusedReadingState class
  */
object FocusedReadingState {

  val featureValueObservations = new mutable.HashMap[String, mutable.ArrayBuffer[Double]]()

  def recordObsevation(values:Map[String, Double]): Unit ={
    for((k, v) <- values){
      // Lazily create the array buffer for the feature
      if(!featureValueObservations.contains(k)){
        featureValueObservations += k -> new mutable.ArrayBuffer[Double]()
      }
      // Log the feature value
      featureValueObservations(k) += v
    }
  }

  /**
    * Computes the feature ranges out of the observed feature values during the run
    * @return Feature ranges
    */
  def observedFeatureRanges():Map[String, (Double, Double)] = {
    Map() ++ featureValueObservations map {
      case (k, v) =>
        k -> (v.min, v.max)
    }
  }

  def featureNames:Set[String] = {
    val dummyState = new FocusedReadingState(
      0,
      0,
      0,
      0,
      0,
      true,
      0,
      0,
      true,
      true,
      Seq(),
      Seq(),
      Seq(),
      0,
      None
    )

    dummyState.toFeatures().keySet
  }
}
