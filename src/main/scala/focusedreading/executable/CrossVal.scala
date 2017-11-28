package focusedreading.executable

import java.io.{File, FileOutputStream, OutputStreamWriter}

import com.typesafe.scalalogging.LazyLogging
import org.clulab.focusedreading.agents.{RedisSQLiteSearchAgent, SearchAgent}
import org.clulab.reach.focusedreading.reinforcement_learning.actions._
import org.clulab.reach.focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import org.clulab.reach.focusedreading.{Connection, Participant}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, LinearApproximationValues}
import org.sarsamora.policy_iteration.td.SARSA
import org.clulab.focusedreading.agents.PolicySearchAgent

import scala.collection.mutable

/**
  * Created by enrique on 07/08/17.
  */
object Crossval extends App with LazyLogging {

  val dirPath = args(0)

  val dir = new File(dirPath)

  val foldFiles = dir.listFiles().filter(f => f.getName.toLowerCase.endsWith(".tsv"))

  val slices:Seq[(Seq[(String, String)], Seq[(String, String)])] = makeCVSlices(foldFiles)


  // CV results
  val numPathsFound = new mutable.ArrayBuffer[Int]()
  val numPapersRead = new mutable.ArrayBuffer[Int]()
  val numQueriesIssued = new mutable.ArrayBuffer[Int]()

  for((trainData, testData) <- slices){
    // Do training
    val trainer = new Trainer(trainData.toIterator)
    val learntPolicy = trainer.run()
    // Do testing
    val tester = new Tester(testData, learntPolicy)
    val (numFound, papersRead, queriesIssued) = tester.run()
    // Collect results
    numPathsFound += numFound
    numPapersRead += papersRead
    numQueriesIssued += queriesIssued
  }

  // Print stats
  val pathsStats = stats(numPathsFound)
  val papersStats = stats(numPapersRead)
  val queriesStats = stats(numQueriesIssued)

  saveData("explore_baseline.csv.prefix", numPathsFound, numPapersRead, numQueriesIssued)

  println()
  println(s"Paths Found\tMean:${pathsStats._1}\tStd Err: ${pathsStats._2}")
  println(s"Papers Read\tMean:${papersStats._1}\tStd Err: ${papersStats._2}")
  println(s"Queries Issued\tMean:${queriesStats._1}\tStd Err: ${queriesStats._2}")
  println()

  def saveData(name:String, paths:Seq[Int], papers:Seq[Int], queries:Seq[Int]) = {
    val lines = (0 until paths.size).map{
      ix =>
        val (p, pp, q) = (paths(ix), papers(ix), queries(ix))
        s"$p,$pp,$q\n"
    }

    val ow = new OutputStreamWriter(new FileOutputStream(name))
    lines foreach ow.write
    ow.close
  }

  def stats(values:Seq[Int]):(Double, Double) = {
    val mean = values.sum / values.size.toDouble
    val serr = Math.sqrt((values.map{_ - mean}.map{ v => Math.pow(v, 2)}.sum)/  (values.size - 1))

    (mean, serr)
  }

  def makeCVSlices(foldFiles:IndexedSeq[File]):Seq[(Seq[(String, String)], Seq[(String, String)])] = {
    val folds = foldFiles map {
      file =>
        io.Source.fromFile(file).getLines().toList.map{
          l =>
            val x = l.split('\t')
            (x.head, x.last)
        }
    }

    val indices = 0.until(foldFiles.size).toList

    val slices = indices map {
      ix =>
        val test = folds(ix)
        val train = indices.filter(_ != ix).flatMap(folds)

        (train, test)
    }

    slices
  }
}

class Trainer(dataSet:Iterator[Tuple2[String, String]]) {


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

  def run():EpGreedyPolicy = {
    val policyIteration = new SARSA(focusedReadingFabric, 20000, 200, alpha = 0.05, gamma = 0.3)
    // TODO: Put the action choice on a better place
    val possibleActions = Set[Action]() ++ PolicySearchAgent.usedActions
    val qFunction = new LinearApproximationValues(possibleActions)
    val initialPolicy = new EpGreedyPolicy(0.5, qFunction)

    val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

    // Store the policy somewhere
    // Serializer.save(learntPolicy, "learnt_policy.ser")
    //learntPolicy.save("learnt_policy.json")

    //    val f = Figure()
    //    val p = f.subplot(0)
    //    val x = linspace(0.0, policyIteration.controlCount.toDouble, policyIteration.controlCount)
    //
    //    val num = qFunction.coefficientsExplore.size
    //    val names = qFunction.coefficientsExplore.keySet.toSeq.sorted
    //    for(i <- 0 until num) {
    //      val history = DenseVector(qFunction.coefficientMemoryExplore.map {
    //        v =>
    //          if(v.length == 0)
    //            0.0
    //          else
    //            v(i)
    //      }.toArray)
    //
    //      p += plot(x, history, '-', null, names(i))
    //    }
    //
    //    p.legend = true
    //    p.xlabel = "Update #"
    //    p.ylabel = "Coef Explore value"
    //
    //    f.saveas("plot_explore.png")

    learntPolicy.asInstanceOf[EpGreedyPolicy]
  }


}


class Tester(dataSet:Iterable[Tuple2[String, String]], policy:EpGreedyPolicy) extends LazyLogging{


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

      //val agent = new LuceneReachSearchAgent(participantA, participantB)
      //val agent = new PolicySearchAgent(participantA, participantB, policy)
      val agent = new RedisSQLiteSearchAgent(participantA, participantB)
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
