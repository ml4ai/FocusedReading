package focusedreading.executable

import breeze.linalg.{DenseVector, linspace}
import breeze.plot.{Figure, plot}
import com.typesafe.config.ConfigFactory
import focusedreading.agents.PolicySearchAgent
import focusedreading.Participant
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
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

  val pairs = io.Source.fromFile(inputPath).getLines
    .map{
      s =>
        val t = s.split("\t").toSeq
        //(t(0), t(1), t(2))
        (t.head, t.last)
    }.toList

  def randomizedPairs = {
    scalaRand.shuffle(pairs)
  }

  val dataSet:Iterator[Tuple2[String, String]] = Iterator.continually(randomizedPairs).flatten

  def focusedReadingFabric():Option[Environment] = {
    if(dataSet.hasNext){
      val episode = dataSet.next
      val participantA = Participant("", episode._1)
      val participantB = Participant("", episode._2)

      Some(new SimplePathEnvironment(participantA, participantB))
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
  val eps = Decays.exponentialDecay(epsilon, lowerBound, pairs.size*(epochs-2), pairs.size).iterator
  ///////////////////
  val initialPolicy = new EpGreedyPolicy(eps, qFunction)

  val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

  // Store the policy somewhere
  // Serializer.save(learntPolicy, "learnt_policy.ser")

  val policyPath = trainingConfig.getString("policyFile")
  learntPolicy.save(policyPath)

//  val steps = policyIteration.controlCount
//  val coefficients = qFunction.coefficients.toSeq
//  for(co <- coefficients){
//    val names = co._2.keySet.toSeq.sorted
//    val title = co._1.toString
//    val memory = qFunction.coefficientMemory(co._1).toArray
//    plotCoefficients(title, steps, names, memory)
//  }

}

