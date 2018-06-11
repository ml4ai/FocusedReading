package focusedreading.executable

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.Participant
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

  val config = ConfigFactory.load()

  implicit val indexPath = LuceneIndexDir(config.getConfig("lucene").getString("annotationsIndex"))
  implicit val sqliteFile: SQLiteFile = SQLiteFile(config.getConfig("informationExtraction").getString("sqlitePath"))

  val trainingConfig = config.getConfig("imitation")
  val mdpConfig = config.getConfig("MDP")
  val supervisionConfig = config.getConfig("expertOracle")

  val inputPath = supervisionConfig.getString("inputFile")

  val trainingPaths = Source.fromFile(inputPath).getLines().toList.map(_.split("\t"))

  def randomizedData = {
    scalaRand.shuffle(trainingPaths)
  }

  val dataSet:Iterator[Array[String]] = Iterator.continually(randomizedData).flatten

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

  val episodeObserver = new EpisodeObserver{
    override def observeIteration(data: IterationObservation): Unit = Unit

    override def episodeFinished(data: EpisodeObservation): Unit = {
      val environment = data.environment.asInstanceOf[SimplePathEnvironment]
      episodeCounts += 1
    }
  }
  ////////////////////////////////////////////////////////

  def imitationLearningFabric() = {
    if(dataSet.hasNext){
      val episodeData = dataSet.next
      val sequence  = episodeData

      Some(SimplePathEnvironment(Participant("", sequence.head), Participant("", sequence.last), sequence map {p => Participant("", p)}, normalizationParameters))
    }
    else
      None
  }


  val epochs = trainingConfig.getInt("epochs")
  val numEpisodes = trainingConfig.getInt("maxEpisodes") //pairs.size * epochs
  val learningRate = trainingConfig.getDouble("initialLearningRate")

  val alphas = Decays.exponentialDecay(learningRate, 0.1, epochs, 0).iterator


  val imitator = new DAgger(imitationLearningFabric, epochs, trainingPaths.size, alphas)
  val activeActions:Set[Action] = PolicySearchAgent.getActiveActions


  // Iterate the policy and it's convergence status
  val learntPolicy:Policy  = imitator.learnPolicy(Some(episodeObserver))

  // Print the number of times the reward was shaped

  // Store the policy somewhere
  val policyPath = trainingConfig.getString("policyFile")
  learntPolicy.save(policyPath)

  // Compute the features' observed  ranges
  val featureRanges = FocusedReadingState.observedFeatureRanges()

  // Store those ranges
  val rangesPath = trainingConfig.getString("rangesFile")
  NormalizationParameters.serializeFeatureRanges(featureRanges, rangesPath)

}

