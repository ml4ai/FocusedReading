package focusedreading.imitation_learning

import focusedreading.policies.{ClassifierPolicy, OraclePolicy}
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import focusedreading.reinforcement_learning.states.FocusedReadingState
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.policy_iteration.EpisodeObserver
import org.sarsamora.states.State
import java.util.Random
import collection.JavaConversions._

import com.typesafe.config.ConfigFactory
import focusedreading.reinforcement_learning.actions
import focusedreading.supervision.search.{LibSVMClassifier, LinearKernel}
import focusedreading.supervision.search.executable.SVMPolicyClassifier
import org.clulab.learning.Datasets.mkTrainIndices
import org.clulab.learning.RVFDataset

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DAgger(episodeFabric:() => Option[(Seq[FocusedReadingAction], SimplePathEnvironment)], epochs:Int, epochSize:Int, alphas:Iterator[Double]) {

  // (state, predicted action, real action)
  private val experience = new mutable.ListBuffer[(FocusedReadingState, FocusedReadingAction, FocusedReadingAction)]
  private val sampler = new Random(0)
  private val possibleActions = Seq(
    ExploreEndpoints_ExploitQuery(),
    ExploreEndpoints_ExploreManyQuery(),
    ExploitEndpoints_ExploitQuery(),
    ExploitEndpoints_ExploreManyQuery()
  )

  // Load the configuration parameters
  val config = ConfigFactory.load()
  val imitationConfig = config.getConfig("imitation")
  val toBeIncluded = imitationConfig.getStringList("includedFeatures").toSet


  def learnPolicy(observer:Option[EpisodeObserver] = None):Policy = {

    // Uniform random number generator from 0 to the number of actions

    var previousPolicy:Option[LibSVMClassifier[FocusedReadingAction, String]] = None


    for(epoch <- 0 to epochs){
      val alpha = alphas.next()
      var oracleTimes = 0
      var policyTimes = 0

      println(s"DAgger info: Epoch $epoch\tAlpha: $alpha\tData set size: ${experience.size}")
      for(ix <- 0 to epochSize){
        val (expertChoices, environment) = episodeFabric().get
        val actionLog = new mutable.ListBuffer[FocusedReadingAction]()

        val consumedExpertChoices = Array.fill(expertChoices.size)(false)

        while(!environment.finishedEpisode && !consumedExpertChoices.forall(identity)){
          val state = environment.observeState.asInstanceOf[FocusedReadingState]

          //val oracleChoice = consumedExpertChoices.dropWhile{case (action, found) => found}.head._1//sampleFromOracle(expertChoices, actionLog)
          val oracleChoice = ((expertChoices zip consumedExpertChoices).zipWithIndex dropWhile {
            case ((candidate, used), ix) => used
          }).head

          consumedExpertChoices(oracleChoice._2) = true

          val r = sampler.nextDouble()
          val selectedAction = r match {
            case x:Double if x <= alpha =>
              // Sample from the expert
              oracleTimes += 1
              oracleChoice._1._1
            case x:Double if x > alpha =>
              // Sample from the policy
              policyTimes += 1
              sampleFromLearned(state, previousPolicy)
          }

          actionLog += selectedAction

          environment.execute(selectedAction)

          val datum = (state, selectedAction, oracleChoice._1._1)
          experience += datum
        }



      }
      println(s"DAgger info: Oracle choices: $oracleTimes\tPolicy choices: $policyTimes")
      // Train the SVM
      val classifier = trainClassifier(experience)

      previousPolicy = Some(classifier)
    }

    new ClassifierPolicy(previousPolicy.get)
  }

  def sampleFromLearned(state:FocusedReadingState, previousPolicy:Option[LibSVMClassifier[FocusedReadingAction, String]]):FocusedReadingAction = previousPolicy match {
    case None =>
      val ix = sampler.nextInt(6)
      possibleActions(ix)
    case Some(classifier) =>
      val features = SVMPolicyClassifier.filterFeatures(state.toFeatures, toBeIncluded)
      val datum = SVMPolicyClassifier.toDatum(features, possibleActions(0))
      classifier.classOf(datum)
  }


  def sampleFromOracle(expertChoices: Seq[FocusedReadingAction], actionLog: Seq[FocusedReadingAction]):FocusedReadingAction = {
    val candidate = expertChoices.head
    if(actionLog.isEmpty)
      candidate
    else {
      val candidateIx = actionLog.indexOf(candidate)
      if (candidateIx == -1)
        candidate
      else
        sampleFromOracle(expertChoices.tail, actionLog.drop(candidateIx + 1))
    }
  }

  def trainClassifier(experience: Iterable[(FocusedReadingState, FocusedReadingAction, FocusedReadingAction)]):LibSVMClassifier[FocusedReadingAction, String] = {
    val data = experience map {
      case(s, _, l) =>
      (SVMPolicyClassifier.filterFeatures(s.toFeatures, toBeIncluded), l)} map SVMPolicyClassifier.toDatum

    val dataset = new RVFDataset[FocusedReadingAction, String]()

    data foreach { dataset += _}

    val classifier = new LibSVMClassifier[FocusedReadingAction, String](LinearKernel, C = 0.1, cacheSize = 200, probability = false)

    val x = dataset.labels map dataset.labelLexicon.get groupBy identity mapValues (_.size)
    val indices = mkTrainIndices(dataset.size, None)

    val frequencies = experience.map(_._3).groupBy(identity).mapValues(_.size)
    val numSamples = experience.size
    val numClasses = frequencies.keySet.size.toDouble

    val weights = Seq(ExploitEndpoints_ExploreManyQuery().asInstanceOf[FocusedReadingAction],
      ExploreEndpoints_ExploreManyQuery().asInstanceOf[FocusedReadingAction],
      ExploitEndpoints_ExploitQuery().asInstanceOf[FocusedReadingAction],
      ExploreEndpoints_ExploitQuery().asInstanceOf[FocusedReadingAction]).map{
      k =>
        frequencies.get(k) match {
          case Some(num)=> k -> numSamples / (numClasses*num)
          case None => k -> 0d
        }

    }.filter{case (k, v) => v != 0}.toMap

    classifier.train(dataset, indices, Some(weights))

    classifier
  }

}
