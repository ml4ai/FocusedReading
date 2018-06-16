package focusedreading.executable

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.{Configuration, Participant}
import focusedreading.agents.{LuceneIndexDir, PolicySearchAgent, SQLiteFile}
import focusedreading.imitation_learning.DAgger
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import focusedreading.reinforcement_learning.states.{FocusedReadingState, NormalizationParameters}
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}
import org.sarsamora.{Decays, scalaRand}

import scala.io.Source


/**
  * Created by enrique on 31/03/17.
  */

object TrainingImitation extends App with LazyLogging {

  type SolutionsMap = Map[(String, String), Option[Seq[(FocusedReadingState, FocusedReadingAction, Double)]]]

  implicit val indexPath: LuceneIndexDir = LuceneIndexDir(Configuration.Lucene.indexPath)
  implicit val sqliteFile: SQLiteFile = SQLiteFile(Configuration.SQLite.dbPath)


  val inputPath = Configuration.Imitation.inputPath

  val trainingPaths = Source.fromFile(inputPath).getLines().toList.map(_.split("\t"))

  def randomizedData = {
    scalaRand.shuffle(trainingPaths)
  }

  val dataSet:Iterator[Array[String]] = Iterator.continually(randomizedData).flatten

  // Instantiate the normalization parameters, if necessary

  val normalizationParameters = if(Configuration.Imitation.Normalization.enabled){

      val lower = Configuration.Imitation.Normalization.lower
      val upper = Configuration.Imitation.Normalization.upper
      val ranges = NormalizationParameters.readFeatureRanges(Configuration.Imitation.Normalization.rangesPath)

      val parameters = NormalizationParameters(lower, upper, ranges)

      Some(parameters)
    }
    else{
      None
    }


  /////////////////////////////////////////////////////////
  // Keep track of policy iteration statistics
  var episodeCounts = 0

  val episodeObserver = new EpisodeObserver{
    override def observeIteration(data: IterationObservation): Unit = Unit

    override def episodeFinished(data: EpisodeObservation): Unit = {
      episodeCounts += 1
    }
  }
  ////////////////////////////////////////////////////////

  def imitationLearningFabric() = {
    if(dataSet.hasNext){
      val episodeData = dataSet.next
      val sequence  = episodeData

      Some(SimplePathEnvironment(Participant.get("", sequence.head.intern()), Participant.get("", sequence.last.intern()), sequence map {p => Participant.get("", p.intern)}, normalizationParameters))
    }
    else
      None
  }


  val epochs = Configuration.Imitation.epochs
  val numEpisodes = Configuration.Imitation.maxEpisodes
  val learningRate = Configuration.Imitation.initialLearningRate

  val alphas = Decays.exponentialDecay(learningRate, 0.1, epochs, 0).iterator


  val imitator = new DAgger(imitationLearningFabric, epochs, trainingPaths.size, alphas)
  val activeActions:Set[Action] = PolicySearchAgent.getActiveActions.toSet


  // Iterate the policy and it's convergence status
  val learntPolicy:Policy  = imitator.learnPolicy(Some(episodeObserver))

  // Print the number of times the reward was shaped

  // Store the policy somewhere
  val policyPath = Configuration.Imitation.policyPath
  learntPolicy.save(policyPath)

  // Compute the features' observed  ranges
  val featureRanges = FocusedReadingState.observedFeatureRanges()

  // Store those ranges
  val rangesPath = Configuration.Imitation.rangesOutputPath
  NormalizationParameters.serializeFeatureRanges(featureRanges, rangesPath)

}

