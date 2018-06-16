package focusedreading.executable.cross_validation

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.agents.{LuceneIndexDir, PolicySearchAgent, SQLiteFile, SearchAgent}
import focusedreading.{Configuration, Connection, Participant}
import org.sarsamora.policies.EpGreedyPolicy

import scala.collection.mutable

class Tester(dataSet:Iterable[Tuple2[String, String]], policy:EpGreedyPolicy) extends LazyLogging{

  implicit val indexDir = LuceneIndexDir(Configuration.Lucene.indexPath)
  implicit val sqliteFile: SQLiteFile = SQLiteFile(Configuration.SQLite.dbPath)

  def getParticipants(path:List[Connection]):List[String] = {
    path match {
      case h::t =>
        h.controller.id :: (if(t == Nil) List(h.controlled.id) else getParticipants(t))
      case Nil => Nil
    }
  }


  def run():(Int, Int, Int) = {
    var (successes, failures, hits) = (0, 0, 0)
    val pathLengths = new mutable.ArrayBuffer[Int]
    val successIterations = new mutable.ArrayBuffer[Int]
    val failureIterations = new mutable.ArrayBuffer[Int]

    logger.info(s"About to do ${dataSet.size} searches ...")
    logger.info(s"")

    val times = new mutable.ArrayBuffer[Long]
    val papers = new mutable.ArrayBuffer[String]
    var numQueries = 0

    val bootstrap = new mutable.HashMap[Int, (Boolean, Int, String)]() // (Success, # queries, papers)

    for((datum, ix) <- dataSet.zipWithIndex){

      val start = System.nanoTime()

      logger.info(s"Searching for path: ${datum._1} - ${datum._2}")

      val participantA =  Participant("", datum._1)
      val participantB = Participant("", datum._2)

      logger.info(s"About to start a focused search $ix of ${dataSet.size}")

      // TODO: Add the normalization parameters to the arguments of the policy search agent
      val agent = new PolicySearchAgent(participantA, participantB, Some(policy))
      agent.focusedSearch(participantA, participantB)


      val recoveredPath = agent.successStopCondition(participantA, participantB, agent.model) match {
        case Some(paths) =>
          successes += 1
          successIterations += agent.iterationNum
          logger.info("Success!!")

          val path = paths.head

          logger.info("")
          logger.info("Path: " + path.mkString(" || "))


          // Analysis
          val participants = getParticipants(path.toList)
          //          val groundTruth = datum.toList
          //          logger.info("GT: " + groundTruth.mkString(" || "))
          //
          //          assert(participants.head == datum._1)
          //          assert(participants.last == datum._2)
          //
          //          val relevantParticipants = participants filter (p => groundTruth.contains(p))
          //          val matchType = if(relevantParticipants == groundTruth) {
          //            hits += 1
          //            logger.info(s"Type: Exact")
          //            "exact"
          //          }
          //          else{
          //            // Get the length of the paths
          //            pathLengths += participants.length
          //            logger.info(s"Type: Alternative")
          //            "alternative"
          //          }

          //serializeItem(path, groundTruth, matchType, agent, new File(s"hits/hit_$ix.json"))


          logger.info("End Success")

          Some(path)

        case None =>
          failures += 1
          failureIterations += agent.iterationNum
          logger.info("Failure")
          None

      }

      // Store the trace for analysis
      //      val trace = AgentRunTrace(participantA, participantB,
      //        agent.trace, recoveredPath, Some(datum))
      //
      //      val tracePath = AgentRunTrace.getFileName(datum)

      var success = true
      //      recoveredPath match {
      //        case Some(_) =>
      //          AgentRunTrace.save(trace, Paths.get("traces", "successes", tracePath))
      //          success = true
      //        case None =>
      //          AgentRunTrace.save(trace, Paths.get("traces", "failures", tracePath))
      //          success = false
      //      }

      numQueries += agent.iterationNum
      val end = System.nanoTime()

      times += (end - start)
      papers ++= agent.papersRead

      bootstrap += (ix -> (success, agent.iterationNum, agent.papersRead.mkString(",")))

      logger.info("")
    }
    logger.info(s"Finished attaining $successes successes and $failures failures")
    logger.info("")
    logger.info("Postmortem analysis:")
    logger.info(s"Exact matches: $hits\t Alternative matches: ${successes-hits}")
    logger.info("")
    // Length counts
    val lengths = pathLengths.groupBy(identity).mapValues(_.size)
    logger.info(s"Alternate path lengths:")
    val keys = lengths.keys.toSeq.sortBy(k => lengths(k))
    for(l <- keys){
      logger.info(s"\tLength $l: ${lengths(l)}")
    }

    logger.info("")
    logger.info("Iteration counts for successes")
    val sIterations = successIterations.groupBy(identity).mapValues(_.size)
    for(l <- sIterations.keys.toSeq.sorted){
      logger.info(s"\t$l iterations: ${sIterations(l)}")
    }

    logger.info("")
    logger.info("Iteration counts for failures")
    val fIterations = failureIterations.groupBy(identity).mapValues(_.size)
    for(l <- fIterations.keys.toSeq.sorted){
      logger.info(s"\t$l iterations: ${fIterations(l)}")
    }


    val averageRuntime = (times.sum / times.size)

    logger.info(s"Average running time: $averageRuntime")
    logger.info(s"Unique papers read: ${papers.toSet.size}")
    logger.info(s"# of queries: $numQueries")

    (successes, papers.toSet.size, numQueries)
  }




  /***
    * Prints the sentences that back the evidence found
    * @param path
    */
  def printEvidence(path: Seq[Connection], agent:SearchAgent) = {
    val evidence:Seq[Iterable[String]] = path map agent.getEvidence

    for((c, e) <- path zip evidence){
      logger.info("")
      logger.info(s"Evidence for connection $c")
      logger.info("")
      for(s <- e){
        logger.info(s)
      }
    }
  }


}
