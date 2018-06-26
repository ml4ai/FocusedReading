package focusedreading.supervision.search.executable

import com.typesafe.scalalogging.LazyLogging
import focusedreading.Configuration
import focusedreading.agents.{LuceneIndexDir, PolicySearchAgent, SQLiteFile}
import focusedreading.entities.{Connection, Participant}
import focusedreading.imitation_learning.{RedisCache, SolutionsCache}
import focusedreading.ir.queries.LuceneQueries
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import focusedreading.sqlite.SQLiteQueries
import focusedreading.implicits._
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.ReferencePathSegment
import focusedreading.supervision.search.{FRSearchState, SearchResult, UniformCostSearch}

import scala.io.Source

/**
  * This program explores the decision tree of a FR search to cache it into redis
  */
//noinspection TypeAnnotation
object SearchTreeExplorer extends App with LazyLogging {

  implicit val indexPath: LuceneIndexDir = LuceneIndexDir(Configuration.Lucene.indexPath)
  implicit val sqliteFile: SQLiteFile = SQLiteFile(Configuration.SQLite.dbPath)

  def readLines(path: String) = Source.fromFile(trainingInputPath).getLines().toList.map(_.split("\t"))

  val trainingInputPath = Configuration.Training.inputPath
  val testingInputPath = Configuration.Testing.inputPath
  val dataSet = readLines(trainingInputPath) ++ readLines(testingInputPath)
  val y = dataSet.iterator

  def buildFabric(s: Iterable[Array[String]]) = {
    val y = s.iterator

    () => {

      if (y.hasNext) {
        val episodeData = y.next
        val sequence = episodeData

        Some(SimplePathEnvironment(
          Participant.get("", sequence.head.intern()),
          Participant.get("", sequence.last.intern()),
          sequence map { p => Participant.get("", p.intern) }, None)
        )
      }
      else
        None
    }
  }

  //  def imitationLearningFabric() = {
  //    if(y.hasNext){
  //      val episodeData = y.next
  //      val sequence  = episodeData
  //
  //      Some(SimplePathEnvironment(
  //        Participant.get("", sequence.head.intern()),
  //        Participant.get("", sequence.last.intern()),
  //        sequence map {p => Participant.get("", p.intern)}, None)
  //      )
  //    }
  //    else
  //      None
  //  }

  // Load the configuration parameters
  private val toBeIncluded = Configuration.Imitation.activeFeatures.toSet
  private val maxIterations = Configuration.MDP.maxIterations

  // Interning strings
  println("Interning strings ...")
  val sqlitePath: String = Configuration.SQLite.dbPath
  val da = new SQLiteQueries(sqlitePath)

  println("Interning PMCIDs...")
  val allPMCIDs: Iterable[String] = da.getAllPMCIDs
  allPMCIDs foreach (_.intern)

  println("Interning participant strings...")
  val allParticipants: Iterable[String] = da.getAllParticipants
  allParticipants foreach (_.intern)

  println("Interning participant instances...")
  allParticipants foreach (p => Participant.get("", p.intern))

  println("Interning connection instances...")
  val allConnections: Iterable[(String, String, Boolean)] = da.getAllInteractions
  allConnections foreach {
    case (controller, controlled, direction) =>
      val pa = Participant.get("", controller.intern)
      val pb = Participant.get("", controlled.intern)
      Connection.get(pa, pb, direction)
  }

  // To avoid a race condition further down
  LuceneQueries.getSearcher(Configuration.Lucene.indexPath)

  def exploreEnvironment(fabric: () => Option[SimplePathEnvironment], num:Int = 1, offset:Int = 0): Unit = {
    fabric() match {
      case Some(environment) =>
        val optimalSequencesCache: SolutionsCache = new RedisCache()
        println(s"Exploring instance ${offset+num} out of ${dataSet.size}")
        traverseTree(environment, optimalSequencesCache)
        exploreEnvironment(fabric, num+1, offset)
      case None => ()
    }
  }

  def traverseTree(environment: SimplePathEnvironment, cache: SolutionsCache): Unit = {

    // TODO: This is duplicated from Dagger.scala. Refactor it
    val reference: Seq[ReferencePathSegment] = environment.referencePath.sliding(2).map {
      r =>
        ReferencePathSegment(r.head.id, r(1).id, Seq.empty[String])
    }.toSeq

    val currentState: FocusedReadingState = environment.observeState
    val agent: PolicySearchAgent = environment.agent

    val actions = environment.possibleActions().map(_.asInstanceOf[FocusedReadingAction])

    def walker(s: FocusedReadingState, a: PolicySearchAgent, as: Seq[FocusedReadingAction]): Unit = {
      logger.info(s"Walking environment $environment at iteration ${a.iterationNum}")
      if(!(cache contains s)) {

        if (!a.hasFinished(environment.participantA, environment.participantB, a.model, false)) {
          for (action <- as) {
            val newAgent = a.clone()
            newAgent.executePolicy(action)

            // TODO check implicits here: the state implicit conversion doesn't work in the argument of walker
            // and the action conversion doesn't work as parameter of the sequence. In the book should be the answer
            val newState: FocusedReadingState = newAgent.observeState
            walker(newState, newAgent, newAgent.possibleActions.map {
              _.asInstanceOf[FocusedReadingAction]
            })
          }
        }

      }
      cacheOptimalSequence(s, a, reference, cache)
    }

    walker(currentState, agent, actions)
  }

  def cacheOptimalSequence(state: FocusedReadingState, agent: PolicySearchAgent, reference: Seq[ReferencePathSegment], cache:SolutionsCache): Unit = {

    // TODO: This is duplicated from Dagger.scala. Refactor it
    def cacheSequence(state: FocusedReadingState, results: Seq[SearchResult]) {
      if (results.nonEmpty) {
        cache.cache(state, results)
        cacheSequence(results.head.state, results.tail)
      }
    }

    if (!(cache contains state)) {
      logger.info("Cache Miss!")
      val searcher = new UniformCostSearch(FRSearchState(agent, reference, agent.iterationNum, maxIterations))
      searcher.solve() match {
        case Some(solution) =>
          val sequence: Seq[SearchResult] = searcher.actionSequence(solution)
          val choice = sequence.head.action
          // Cache all the nested subsequences from the current answer
          cacheSequence(state, sequence)
        case None =>
          val choice = agent.possibleActions.randomElement
          val sequence = Seq(SearchResult(state, choice, agent paperAmountFor choice, None, None))
          cache.cache(state, sequence)
      }
    }
    else
      logger.info("Cache Hit!")
  }

  for ((segment, ix) <- dataSet.grouped(100).toList.zipWithIndex.par) {
    val fabric = buildFabric(segment)
    exploreEnvironment(fabric, ix)
  }
}
