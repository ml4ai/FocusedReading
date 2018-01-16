package focusedreading.reinforcement_learning.states

import java.io.File

import org.apache.commons.io.FileUtils
import scala.collection.JavaConverters._

/**
  * Encapsulates feature normalization following the svm-scale approach.
  * Inspired on https://github.com/clulab/processors/blob/master/main/src/main/scala/org/clulab/learning/Datasets.scala
  *
  * @param lower bound to the normalized range
  * @param upper bound to the normalize range
  * @param ranges Observed ranges of the unormalized features during a training session
  */
case class NormalizationParameters(lower:Double, upper:Double, ranges:Map[String, (Double, Double)]) {

  /**
    * Normalizes the feature map given the parameters
    * @param features to be normalized
    * @return Isomorphic map with normalized values
    */
  def normalize(features:Map[String, Double]):Map[String, Double] = {
    Map() ++ features map {
      case (k, v) =>
        // If there's no observed range, then return the original value
        if(!ranges.contains(k)) {
          k -> v
        }
        else{
          val (min, max) = ranges(k)
          k -> this.scale(v, min, max)
        }
    }
  }

  /**
    * Scales a feature value given the parameters
    * @param value observed value
    * @param min minimum observed instance during training
    * @param max maximum observed instance during training
    * @return scaled value using svm-scale formula
    */
  private def scale(value:Double, min:Double, max:Double): Double = {
    if(min == max) return upper
    // the result will be a value in [lower, upper]
    lower + (upper - lower) * (value - min) / (max - min)
  }
}

/**
  * Companion object with auxiliary functions
  */
object NormalizationParameters{

  /**
    * Reads the observed ranges values out of a tsv file
    * @param path to the tsv file
    * @return Map with the observed ranges
    */
  def readFeatureRanges(path:String):Map[String, (Double, Double)] = {
    val source = io.Source.fromFile(path)
    val ranges = source.getLines().map{
      l =>
        val tokens = l.split('\t')
        val (k, min, max) = (tokens(0), tokens(1).toDouble, tokens(2).toDouble)

        k -> (min, max)
    }.toMap
    source.close()

    ranges
  }

  /**
    * Saves the observed features range into a tsv file
    * @param featureRanges observed feature ranges
    * @param path file location
    */
  def serializeFeatureRanges(featureRanges:Map[String, (Double, Double)], path:String): Unit ={
    val lines = featureRanges map {
      case (k, v) => s"$k\t${v._1}\t${v._2}"
    }

    FileUtils.writeLines(new File(path), lines.asJavaCollection)
  }


}