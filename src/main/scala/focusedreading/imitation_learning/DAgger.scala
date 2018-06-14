package focusedreading.imitation_learning

import java.util.Random

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.ir.LuceneQueries
import focusedreading.policies.ClassifierPolicy
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.sqlite.SQLiteQueries
import focusedreading.supervision.ReferencePathSegment
import focusedreading.supervision.search.executable.{DoSearch, SVMPolicyClassifier}
import focusedreading.supervision.search._
import focusedreading.{Connection, Participant}
import org.clulab.learning.Datasets.mkTrainIndices
import org.clulab.learning.RVFDataset
import org.sarsamora.policies.Policy
import org.sarsamora.policy_iteration.EpisodeObserver

import scala.collection.JavaConversions._
import scala.collection.mutable

class DAgger(episodeFabric: => Option[SimplePathEnvironment], epochs:Int, epochSize:Int, alphas:Iterator[Double]) extends LazyLogging{

  // Load the configuration parameters
  private val config = ConfigFactory.load()
  private val imitationConfig = config.getConfig("imitation")
  private val toBeIncluded = imitationConfig.getStringList("includedFeatures").toSet
  private val maxIterations = config.getConfig("MDP").getInt("maxIterations")

  // Interning strings
  println("Interning strings ...")
  val sqlitePath = config.getConfig("informationExtraction").getString("sqlitePath")
  val da = new SQLiteQueries(sqlitePath)

  println("Interning PMCIDs...")
  val allPMCIDs = da.getAllPMCIDs
  allPMCIDs foreach (_.intern)

  println("Interning participant strings...")
  val allParticipants = da.getAllParticipants
  allParticipants foreach (_.intern)

  println("Interning participant instances...")
  allParticipants foreach (p => Participant.get("", p.intern))

  println("Interning connection instances...")
  val allConnections = da.getAllInteractions
  allConnections foreach {
    case (controller, controlled, direction) =>
      val pa = Participant.get("", controller.intern)
      val pb = Participant.get("", controlled.intern)
      Connection.get(pa, pb, direction)
  }

  // To avoid a race condition further down
  LuceneQueries.getSearcher(config.getConfig("lucene").getString("annotationsIndex"))

  // (state, predicted action, real action)
  private val experience = new mutable.ListBuffer[(FocusedReadingState, FocusedReadingAction, FocusedReadingAction)]
  private val sampler = new Random(0)
  private val possibleActions = Seq(
    ExploreEndpoints_ExploitQuery,
    ExploreEndpoints_ExploreManyQuery,
    ExploitEndpoints_ExploitQuery,
    ExploitEndpoints_ExploreManyQuery
  )

  private val optimalSequencesCache:SolutionsCache = new RedisCache()//new MapCache()

  def askExpert(environment: SimplePathEnvironment): FocusedReadingAction = {

    import focusedreading.implicits._

    val agent = environment.agent
    val state = agent.observeState.asInstanceOf[FocusedReadingState]

    val reference:Seq[ReferencePathSegment] = environment.referencePath.sliding(2).map{
      r =>
        ReferencePathSegment(r.head.id, r(1).id, Seq.empty[String])
    }.toSeq

    // Look up the cache
    optimalSequencesCache get state match {
      case Some(choice) =>
        logger.info(s"Cache Hit")
        choice.head.action
      case None =>
        logger.info(s"Cache Miss")
        // Doesn't contain it, hence running UCS to find it
        val searcher = new UniformCostSearch(FRSearchState(agent, reference, 0, maxIterations))
        searcher.solve() match {
          case Some(solution) =>

            val sequence: Seq[SearchResult] = DoSearch.actionSequence(solution, searcher)
            val choice = sequence.head.action
            optimalSequencesCache.cache(state, sequence)
            choice
          case None =>
            val choice = agent.possibleActions.randomElement
            val sequence = Seq(SearchResult(state, choice, agent paperAmountFor choice, None, None))
            optimalSequencesCache.cache(state, sequence)
            ExploreEndpoints_ExploitQuery
        }
    }
  }

  def learnPolicy(observer:Option[EpisodeObserver] = None):Policy = {

    // Import the implicit conversions
    import focusedreading.implicits._


    var previousPolicy:Option[LibSVMClassifier[FocusedReadingAction, String]] = None


    for(epoch <- 0 to epochs){
      val alpha = alphas.next()
      var oracleTimes = 0
      var policyTimes = 0

      logger.info(s"Starting Epoch ${epoch+1} of $epochs\tAlpha: $alpha\tData set size: ${experience.size}")

      for(ix <- 0 until epochSize){

        logger.info(s"Epoch ${epoch+1} of $epochs\tIteration: ${ix+1} of $epochSize")

        episodeFabric match {
          case Some(environment) =>
            val actionLog = new mutable.ListBuffer[FocusedReadingAction]()

            while(!environment.finishedEpisode){
              val state:FocusedReadingState = environment.observeState

              val oracleChoice = askExpert(environment)

              val r = sampler.nextDouble()
              val selectedAction = r match {
                case x:Double if x <= alpha =>
                  // Sample from the expert
                  oracleTimes += 1
                  oracleChoice
                case x:Double if x > alpha =>
                  // Sample from the policy
                  policyTimes += 1
                  sampleFromLearned(state, previousPolicy)
              }

              actionLog += selectedAction

              environment.execute(selectedAction)

              val datum = (state, selectedAction, oracleChoice)
              experience += datum
             }

        case None => Unit

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

    val weights = Seq(ExploitEndpoints_ExploreManyQuery.asInstanceOf[FocusedReadingAction],
      ExploreEndpoints_ExploreManyQuery.asInstanceOf[FocusedReadingAction],
      ExploitEndpoints_ExploitQuery.asInstanceOf[FocusedReadingAction],
      ExploreEndpoints_ExploitQuery.asInstanceOf[FocusedReadingAction]).map{
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
