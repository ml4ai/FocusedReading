package focusedreading.executable

import java.io.File

import collection.mutable
import breeze.linalg.{DenseVector, linspace}
import breeze.plot.{Figure, plot}
import org.apache.commons.io.FileUtils
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.agents.{LuceneIndexDir, PolicySearchAgent, SQLiteFile}
import focusedreading.Participant
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import focusedreading.reinforcement_learning.states.{FocusedReadingState, NormalizationParameters}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, Policy}
import org.sarsamora.policy_iteration.td.value_functions.LinearApproximationActionValues
import org.sarsamora.policy_iteration.td.{QLearning, SARSA}
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}
import org.sarsamora.{Decays, scalaRand}


/**
  * Created by enrique on 31/03/17.
  */

// TODO: Clean up this class
object Training extends App with LazyLogging {

  val config = ConfigFactory.load()

  val trainingConfig = config.getConfig("training")
  val mdpConfig = config.getConfig("MDP")

  val inputPath = trainingConfig.getString("inputFile")
  implicit val indexPath = LuceneIndexDir(config.getConfig("lucene").getString("annotationsIndex"))
  implicit val sqliteFile: SQLiteFile = SQLiteFile(config.getConfig("informationExtraction").getString("sqlitePath"))


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

  val dataSet:Iterator[((String, String), Seq[String])] = Iterator.continually(randomizedData).flatten


  // Instantiate the normalization parameters, if necessary
  val normalizationConfig = trainingConfig.getConfig("normalization")

  val normalizationParameters = if(normalizationConfig.getBoolean("enabled")){

      val lower = normalizationConfig.getDouble("lower")
      val upper = normalizationConfig.getDouble("upper")
      val ranges = NormalizationParameters.readFeatureRanges(normalizationConfig.getString("rangesFile"))

      val parameters = NormalizationParameters(lower, upper, ranges)

      Some(parameters)
    }
    else{
      None
    }


  /////////////////////////////////////////////////////////
  // Keep track of policy iteration statistics
  var episodeCounts = 0
  var shapedEpisodes = 0


  val episodeObserver = new EpisodeObserver{
    override def observeIteration(data: IterationObservation): Unit = Unit

    override def episodeFinished(data: EpisodeObservation): Unit = {
      val environment = data.environment.asInstanceOf[SimplePathEnvironment]
//      if(environment.rewardShapped > 0){
//        shapedEpisodes += 1
//      }
      episodeCounts += 1
    }
  }
  ////////////////////////////////////////////////////////

  val focusedReadingFabric = () => {
    if(dataSet.hasNext){
      val episodeData = dataSet.next
      val (pair, sequence) = episodeData
      val participantA = Participant("", pair._1)
      val participantB = Participant("", pair._2)
      val reference = sequence map (p => Participant("", p))

      Some(SimplePathEnvironment(participantA, participantB, reference, normalizationParameters))
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
  val lambda = 0.9d // TODO: parameterize this

  // TODO: Delete me
  val alphaDecrease = learningRate/numEpisodes
  val alphas = (0 to numEpisodes).toStream.map(i => learningRate-(i*alphaDecrease)).iterator
  //////////////////////////

  val policyIteration = new QLearning(focusedReadingFabric, numEpisodes, burnInEpisodes, alphas, decay, lambda)
  val activeActions:Set[Action] = PolicySearchAgent.getActiveActions.toSet
  val qFunction = new LinearApproximationActionValues(activeActions, FocusedReadingState.featureNames, true)

  // Decaying epsilon
  val epsilon = trainingConfig.getConfig("epsilon").getDouble("initial")
  val lowerBound = trainingConfig.getConfig("epsilon").getDouble("lowerBound")
//  val epsilonDecrease = (epsilon-0.01)/(numEpisodes/2.0)
//  val eps = (0 to (numEpisodes/2)).toStream.map(i => epsilon-(i*epsilonDecrease)).iterator ++ Stream.continually(0.01)
  val eps = Decays.exponentialDecay(epsilon, lowerBound, trainingData.size*(epochs-2), trainingData.size).iterator
  ///////////////////

  // Initial policy
  val initialPolicy = new EpGreedyPolicy(eps, qFunction)

  // Iterate the policy and it's convergence status
  val (learntPolicy:Policy, convergenceStatus) = policyIteration.iteratePolicy(initialPolicy, Some(episodeObserver))

  // Print the number of times the reward was shaped

  // Store the policy somewhere
  val policyPath = trainingConfig.getString("policyFile")
  learntPolicy.save(policyPath)

  // Compute the features' observed  ranges
  val featureRanges = FocusedReadingState.observedFeatureRanges()

  // Store those ranges
  val rangesPath = trainingConfig.getString("rangesFile")
  NormalizationParameters.serializeFeatureRanges(featureRanges, rangesPath)


//  val steps = policyIteration.controlCount
//  val coefficients = qFunction.coefficients.toSeq
//  for(co <- coefficients){
//    val names = co._2.keySet.toSeq.sorted
//    val title = co._1.toString
//    val memory = qFunction.coefficientMemory(co._1).toArray
//    plotCoefficients(title, steps, names, memory)
//  }

}

