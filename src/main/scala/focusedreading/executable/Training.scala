package focusedreading.executable

import breeze.linalg.{DenseVector, linspace}
import breeze.plot.{Figure, plot}
import com.typesafe.scalalogging.LazyLogging
import focusedreading.agents.{LuceneIndexDir, PolicySearchAgent, SQLiteFile}
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import focusedreading.reinforcement_learning.states.{FocusedReadingState, NormalizationParameters}
import focusedreading.{Configuration, Participant}
import org.sarsamora.actions.Action
import org.sarsamora.policies.{EpGreedyPolicy, Policy}
import org.sarsamora.policy_iteration.td.QLearning
import org.sarsamora.policy_iteration.td.value_functions.LinearApproximationActionValues
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}
import org.sarsamora.{Decays, scalaRand}


/**
  * Created by enrique on 31/03/17.
  */

// TODO: Clean up this class
object Training extends App with LazyLogging {

  val inputPath = Configuration.Training.inputPath
  implicit val indexPath = LuceneIndexDir(Configuration.Lucene.indexPath)
  implicit val sqliteFile: SQLiteFile = SQLiteFile(Configuration.SQLite.dbPath)


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
  val normalizationParameters = if(Configuration.Training.Normalization.enabled){

      val lower = Configuration.Training.Normalization.lower
      val upper = Configuration.Training.Normalization.upper
      val ranges = NormalizationParameters.readFeatureRanges(Configuration.Training.Normalization.rangesPath)

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

  val epochs = Configuration.Training.epochs
  val numEpisodes = Configuration.Training.maxEpisodes
  val burnInEpisodes = Configuration.Training.burnInEpisodes
  val learningRate = Configuration.Training.learningRate
  val decay = Configuration.Training.decayParameter
  val lambda = 0.9d // TODO: parameterize this

  // TODO: Delete me
  val alphaDecrease = learningRate/numEpisodes
  val alphas = (0 to numEpisodes).toStream.map(i => learningRate-(i*alphaDecrease)).iterator
  //////////////////////////

  val policyIteration = new QLearning(focusedReadingFabric, numEpisodes, burnInEpisodes, alphas, decay, lambda)
  val activeActions:Set[Action] = PolicySearchAgent.getActiveActions.toSet
  val qFunction = new LinearApproximationActionValues(activeActions, FocusedReadingState.featureNames, true)

  // Decaying epsilon
  val epsilon = Configuration.Training.Epsilon.initial
  val lowerBound = Configuration.Training.Epsilon.lowerBound
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
  val policyPath = Configuration.Training.policyPath
  learntPolicy.save(policyPath)

  // Compute the features' observed  ranges
  val featureRanges = FocusedReadingState.observedFeatureRanges()

  // Store those ranges
  val rangesPath = Configuration.Training.outputRangesFile
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

