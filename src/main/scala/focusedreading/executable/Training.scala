package focusedreading.executable

import java.io.File

import breeze.linalg.{DenseVector, linspace}
import breeze.plot.{Figure, plot}
import org.apache.commons.io.FileUtils
import com.typesafe.config.ConfigFactory
import focusedreading.agents.PolicySearchAgent
import focusedreading.Participant
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import focusedreading.reinforcement_learning.states.{FocusedReadingState, NormalizationParameters}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, LinearApproximationValues}
import org.sarsamora.policy_iteration.td.SARSA
import org.sarsamora.{Decays, scalaRand}


/**
  * Created by enrique on 31/03/17.
  */

object Training extends App {

  val config = ConfigFactory.load()

  val trainingConfig = config.getConfig("training")

  val inputPath = trainingConfig.getString("inputFile")


  val trainingData = io.Source.fromFile(inputPath).getLines
    .map{
      s =>
        val t = s.split("\t").toSeq
        //(t(0), t(1), t(2))
        ((t.head, t.last), t)
    }.toList

  def randomizedData = {
    scalaRand.shuffle(trainingData)
  }

  val dataSet:Iterator[Tuple2[(String, String), Seq[String]]] = Iterator.continually(randomizedData).flatten


  // Instantiate the normalization parameters, if necessary
  val normalizationConfig = trainingConfig.getConfig("normalization")

  val normalizationParameters = normalizationConfig.getBoolean("enabled") match {
    case true => {
      val lower = normalizationConfig.getDouble("lower")
      val upper = normalizationConfig.getDouble("upper")
      val ranges = NormalizationParameters.readFeatureRanges(normalizationConfig.getString("rangesFile"))

      val parameters = NormalizationParameters(lower, upper, ranges)

      Some(parameters)
    }
    case false => None
  }
  /////////////////////////////////////////////////////////

  def focusedReadingFabric():Option[Environment] = {
    if(dataSet.hasNext){
      val episodeData = dataSet.next
      val (pair, sequence) = episodeData
      val participantA = Participant("", pair._1)
      val participantB = Participant("", pair._2)
      val reference = sequence map (p => Participant("", p))

      Some(new SimplePathEnvironment(participantA, participantB, reference, normalizationParameters))
    }
    else
      None
  }

  private def plotCoefficients(title:String, steps:Int, names:Seq[String], memory:Array[DenseVector[Double]]) = {
    val f = Figure()
    val p = f.subplot(0)
    val x = linspace(0.0, steps.toDouble, steps)

    val num = names.size

    for (i <- 0 until num) {
      val history = DenseVector(memory.map {
        v =>
          if (v.length == 0)
            0.0
          else
            v(i)
      })

      p += plot(x, history, '-', null, names(i))
    }

    p.legend = true
    p.xlabel = "Update #"
    p.ylabel = "Coef Explore value"
    p.title = title

    f.saveas(s"plot_$title.png")
  }

  val epochs = trainingConfig.getInt("epochs")
  val numEpisodes = trainingConfig.getInt("maxEpisodes") //pairs.size * epochs
  val burnInEpisodes = trainingConfig.getInt("burnInEpisodes")
  val learningRate = trainingConfig.getDouble("learningRate")
  val decay = trainingConfig.getDouble("decayParameter")

  val policyIteration = new SARSA(focusedReadingFabric, numEpisodes, burnInEpisodes, learningRate, decay)
  val activeActions:Set[Action] = PolicySearchAgent.getActiveActions
  val qFunction = new LinearApproximationValues(activeActions)

  // Decaying epsilon
  val epsilon = trainingConfig.getConfig("epsilon").getDouble("initial")
  val lowerBound = trainingConfig.getConfig("epsilon").getDouble("lowerBound")
//  val epsilonDecrease = (epsilon-0.01)/(numEpisodes/2.0)
//  val eps = (0 to (numEpisodes/2)).toStream.map(i => epsilon-(i*epsilonDecrease)).iterator ++ Stream.continually(0.01)
  val eps = Decays.exponentialDecay(epsilon, lowerBound, trainingData.size*(epochs-2), trainingData.size).iterator
  ///////////////////
  val initialPolicy = new EpGreedyPolicy(eps, qFunction)

  val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

  // Store the policy somewhere
  // Serializer.save(learntPolicy, "learnt_policy.ser")

  val policyPath = trainingConfig.getString("policyFile")
  learntPolicy.save(policyPath)

  // Compute the features' observed  ranges
  val featureRanges = FocusedReadingState.observedFeatureRanges()

  // Store those ranges
  val rangesPath = trainingConfig.getString("rangesFile")
  NormalizationParameters.serializeFeatureRanges(featureRanges, "ranges.tsv")


//  val steps = policyIteration.controlCount
//  val coefficients = qFunction.coefficients.toSeq
//  for(co <- coefficients){
//    val names = co._2.keySet.toSeq.sorted
//    val title = co._1.toString
//    val memory = qFunction.coefficientMemory(co._1).toArray
//    plotCoefficients(title, steps, names, memory)
//  }

}

