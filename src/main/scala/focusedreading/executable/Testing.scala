package focusedreading.executable

import java.io.{BufferedWriter, FileOutputStream, FileWriter, _}
import java.nio.file.Paths

import com.typesafe.scalalogging.LazyLogging
import focusedreading.agents.{PolicySearchAgent, SearchAgent}
import focusedreading.reinforcement_learning.actions.FocusedReadingActionValues
import focusedreading.sqlite.SQLiteQueries
import focusedreading.tracing.AgentRunTrace
import focusedreading.{Connection, Participant}
import org.sarsamora.policies._
import com.typesafe.config.ConfigFactory
import focusedreading.reinforcement_learning.states.NormalizationParameters

import scala.collection.mutable

/**
  * Created by enrique on 03/04/17.
  */
object Testing extends App with LazyLogging{

  // to set a custom conf file add -Dconfig.file=/path/to/conf/file to the cmd line for sbt
  val config = ConfigFactory.load()
  val testingConfig = config.getConfig("testing")
  val outputConfig = testingConfig.getConfig("output")

  def getParticipants(path:List[Connection]):List[String] = {
    path match {
      case h::t =>
        h.controller.id :: (if(t == Nil) List(h.controlled.id) else getParticipants(t))
      case Nil => Nil
    }
  }



  val inputPath = testingConfig.getString("inputFile")

  val dataSet:Iterable[Seq[String]] = io.Source.fromFile(inputPath).getLines
    .map{
      s =>
        val t = s.split("\t").toSeq
        //(t(0), t(1), t(2))
        t
    }.toSeq

  // For evaluation purposes
  // Store a key for each interaction, defined as the pair of elements and the list of pathways where they belong to
  val interactionsToAnnotate = new mutable.HashMap[Tuple3[String, String, Boolean], mutable.ArrayBuffer[String]]

  var (successes, failures, hits) = (0, 0, 0)
  val pathLengths = new mutable.ArrayBuffer[Int]
  val successIterations = new mutable.ArrayBuffer[Int]
  val failureIterations = new mutable.ArrayBuffer[Int]

  logger.info(s"About to do ${dataSet.size} searches ...")
  logger.info(s"")


  /***
    * Prints the sentences that back the evidence found
    * @param path Connections that comprise the path
    */
  def printEvidence(path: Seq[Connection], agent:SearchAgent, writer:OutputStreamWriter): Unit = {
    val evidence:Seq[Iterable[String]] = path map agent.getEvidence

    writer.write(s"${path.map(_.toString(humanFriendly = true)).mkString(" - ")}\n")

    for((c, e) <- path zip evidence){
      writer.write("")
      writer.write(s"Evidence for connection ${c.toString(humanFriendly = true)}\n")
      writer.write("")
      for(s <- e){
        writer.write(s)
      }
    }

    writer.write("\n==========\n")
  }

  val valueLoader = new FocusedReadingActionValues

  val times = new mutable.ArrayBuffer[Long]
  val papers = new mutable.ArrayBuffer[String]
  var numQueries = 0
  val actionCounts = new mutable.HashMap[String, Int]()
  val ep = new mutable.ArrayBuffer[((Participant, Participant),(Participant, Participant))]()

  val bootstrap = new mutable.HashMap[Int, (Boolean, Int, String)]() // (Success, # queries, papers)

  val writer = new OutputStreamWriter(new FileOutputStream(outputConfig.getString("evidence")))

  for((datum, ix) <- dataSet.zipWithIndex){

    val start = System.nanoTime()

    logger.info(s"Searching for path: ${datum.mkString(" - ")}")

    val participantA =  Participant("", datum.head)
    val participantB = Participant("", datum.last)

    logger.info(s"About to start a focused search $ix of ${dataSet.size}")

    //val agent = new LuceneReachSearchAgent(participantA, participantB)
    val policyPath = testingConfig.getString("policyFile")
    val policy = Policy.loadPolicy(policyPath, valueLoader).asInstanceOf[EpGreedyPolicy].makeGreedy


    // Instantiate the normalization parameters, if necessary
    val normalizationConfig = testingConfig.getConfig("normalization")

    val normalizationParameters = normalizationConfig.getBoolean("enabled") match {
      case true => {
        val lower = normalizationConfig.getDouble("lower")
        val upper = normalizationConfig.getDouble("upper")
        val ranges = NormalizationParameters.readFeatureRanges(normalizationConfig.getString("rangesFile"))

        val parameters = NormalizationParameters(lower, upper, ranges)

        Some(parameters)
      }
      case false => None
    }
    /////////////////////////////////////////////////////////

    val agent = new PolicySearchAgent(participantA, participantB, policy, normalizationParameters = normalizationParameters)
    // val agent = new SQLiteMultiPathSearchAgent(participantA, participantB)
    agent.focusedSearch(participantA, participantB)


    val recoveredPath = agent.successStopCondition(participantA, participantB, agent.model) match {
      case Some(paths) =>
        successes += 1
        successIterations += agent.iterationNum
        logger.info("Success!!")

        val path = paths.head

        val pathName = path.mkString(" || ")

        this.printEvidence(path, agent, writer)

        logger.info("")
        logger.info("Path: " + pathName)


        // Analysis
        val participants = getParticipants(path.toList)
        val groundTruth = datum.toList
        logger.info("GT: " + groundTruth.mkString(" || "))

        assert(participants.head == groundTruth.head)
        assert(participants.last == groundTruth.last)

        val relevantParticipants = participants filter (p => groundTruth.contains(p))
        val matchType = if(relevantParticipants == groundTruth) {
          hits += 1
          logger.info(s"Type: Exact")
          "exact"
        }
        else{
          // Get the length of the paths
          pathLengths += participants.length
          logger.info(s"Type: Alternative")
          "alternative"
        }


        // For annotation purposes
        for(interaction <- path){
          val key = (interaction.controller.id, interaction.controlled.id, interaction.sign)
          if(interactionsToAnnotate.contains(key)){
            interactionsToAnnotate(key) += pathName
          }
          else{
            val value = new mutable.ArrayBuffer[String]()
            value += pathName
            interactionsToAnnotate += (key -> value)
          }
        }
        //////////////////////////


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
    val trace = AgentRunTrace(participantA, participantB,
      agent.trace, recoveredPath, Some(datum))

    val tracePath = AgentRunTrace.getFileName(datum)

    var success = true
    recoveredPath match {
      case Some(_) =>
        AgentRunTrace.save(trace, Paths.get("traces", "successes", tracePath))
        success = true
      case None =>
        AgentRunTrace.save(trace, Paths.get("traces", "failures", tracePath))
        success = false
    }

    numQueries += agent.iterationNum
    val end = System.nanoTime()

    times += (end - start)
    papers ++= agent.papersRead

    bootstrap += (ix -> (success, agent.iterationNum, agent.papersRead.mkString(",")))

    agent.actionCounters foreach {
      case (k, v) =>
        if(actionCounts.contains(k))
          actionCounts(k) += v
        else
          actionCounts += k -> v
    }

    agent.chosenEndpointsLog foreach {
      x=>
        ep += x
    }

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


  val averageRuntime = times.sum / times.size

  // Store info for bootstrapping


  logger.info(s"Average running time: $averageRuntime")
  logger.info(s"Unique papers read: ${papers.toSet.size}")
  logger.info(s"# of queries: $numQueries")

  val bootstrapLines = bootstrap.map {
    case (ix, v) =>
      s"$ix\t${v._1}\t${v._2}\t${v._3}\n"
  }


  logger.info("")
  logger.info(s"Chosen action counts")
  actionCounts foreach {
    case (a, c) =>
      logger.info(s"$a: $c")
  }

  logger.info("")
  logger.info("Explore/Exploit endpoiny analysis:")

  var same, different = 0
  for((explore, exploit) <- ep){
    val (exploreA, exploreB) = explore
    val (exploitA, exploitB) = exploit

    if(exploreA == exploitA
      && exploreB == exploitB)
      same += 1
    else
      different += 1
  }

  logger.info(s"Same outcome: $same")
  logger.info(s"Different outcome: $different")

  val osw = new BufferedWriter(new FileWriter(outputConfig.getString("bootstrap")))

  bootstrapLines foreach osw.write

  osw.close()

  writer.close()

  // Write down the annotations
  // First, translate the interaction pairs to their PK in the SQLite DB
  val sqlitePath = config.getConfig("informationExtraction").getString("sqlitePath")
  val daIE = new SQLiteQueries(sqlitePath)
  val mappedInteractions = interactionsToAnnotate.map{
    case(key, value) =>
      val id = daIE.getInteractionId(key)
      id -> value
  }

  // Generate the lines and write them down
  val annotationSw = new BufferedWriter(new FileWriter(outputConfig.getString("annotations")))
  mappedInteractions foreach {
    case (key, value) =>
      val cols = value.mkString(",")
      annotationSw.write(s"$key,$cols\n")
  }
  annotationSw.close()

  val papersSw = new BufferedWriter(new FileWriter("read_paper_ids.txt"))
  papers.toSet foreach {
    paperId:String =>
      papersSw.write(s"$paperId\n")
  }
  papersSw.close()
}
