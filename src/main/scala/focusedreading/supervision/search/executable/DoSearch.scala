package focusedreading.supervision.search.executable

import java.io.{FileOutputStream, ObjectOutputStream}

import com.typesafe.config.{Config, ConfigFactory}
import focusedreading.agents.PolicySearchAgent
import focusedreading.ir.LuceneQueries
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.sqlite.SQLiteQueries
import focusedreading.supervision.CreateExpertOracle
import focusedreading.supervision.search.FRSearchState.GoldDatum
import focusedreading.supervision.search.{AStar, FRSearchState, Node, UniformCostSearch}
import focusedreading.{Connection, Participant}
import org.clulab.utils.Serializer

import scala.collection.mutable
import scala.io.Source


object DoSearch extends App{
                // State, action, Cost, estimated remaining cost, Actual Remaining cost
  type Result = (FocusedReadingState, FocusedReadingAction, Int, Int, Int)

  def persistResults(results:Map[(String, String), Option[Seq[Result]]], path:String){
    val osw = new ObjectOutputStream(new FileOutputStream(path))
    osw.writeObject(results)
    osw.close()
  }

  def deserializeResults(path:String):Map[(String, String), Option[Seq[(FocusedReadingState, FocusedReadingAction, Double)]]] = {
    Serializer.load[Map[(String, String), Option[Seq[(FocusedReadingState, FocusedReadingAction, Double)]]]](path)
  }

  // Interning strings
  println("Interning strings ...")
  val config: Config = ConfigFactory.load()
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
      val pa = Participant.get("", controller)
      val pb = Participant.get("", controlled)
      Connection.get(pa, pb, direction)
  }

  // To avoid a race condition further down
  LuceneQueries.getSearcher(config.getConfig("lucene").getString("annotationsIndex"))

  def actionSequence(node:Node):List[Result] = {
    if(node.parent.isDefined){
      if(node.action.isDefined){
        val state = node.state
        val frState = state.agent.observeState.asInstanceOf[FocusedReadingState]
        (frState, node.action.get, node.pathCost, node.estimatedRemaining, node.remaining) :: actionSequence(node.parent.get)
      }
      else{
        actionSequence(node.parent.get)
      }
    }
    else{
      Nil
    }
  }


  private val configuration = ConfigFactory.load()
  val maxIterations = configuration.getConfig("MDP").getInt("maxIterations")
  val stepSize = configuration.getConfig("MDP").getConfig("paperAmounts").getDouble("many")
  val trainingFile = configuration.getConfig("expertOracle").getString("inputFile")
  val fragmentsFile = configuration.getConfig("expertOracle").getString("goldenDataPath")
  //val maxThreads = config.getConfig("search").getInt("maxThreads")

  //val threadSupport = new ForkJoinTaskSupport()

  val trainingPaths = Source.fromFile(trainingFile).getLines().toList.map(_.split("\t")).map(s => s.sliding(2).toList)

  val groundTruth: Map[(String, String), Option[GoldDatum]] = CreateExpertOracle.deserialize(fragmentsFile)

  val trainingData = trainingPaths.map{
    s =>
      val key = (s.head(0), s.last(1))
      val sequence = s.flatMap{
        p =>
          groundTruth((p(0), p(1))).get
      }

      key -> sequence
  }.toMap

  val solutions = new mutable.HashMap[(String, String), Option[Seq[Result]]]()

  val total = trainingData.size

  val start = System.currentTimeMillis()

  val collection = trainingData.keys.toSeq.zipWithIndex


  for((k, ix) <- collection.par){

    println(s"${ix+1} out of $total.\t$k")

    val path = trainingData(k)

    val (participantA, participantB) = (Participant.get("", path.head._1), Participant.get("", path.last._2))

    val agent = new PolicySearchAgent(participantA, participantB)

    val initialState = FRSearchState(agent, path, 0, maxIterations) // TODO: Fix me
    //val solver = new UniformCostSearch(initialState)
    //val solver = new IterativeLengtheningSearch(agent, path, stepSize*10, stepSize, stepSize*100)
    val solver = new AStar(initialState)

    val result = solver.solve()

    solutions(k) = result match {
      case Some(r) => Some(actionSequence(r))
      case None => None
    }
  }

  val end = System.currentTimeMillis()

  println()
  println(s"${(end - start)/1000} senconds")
  println(s"Found ${solutions.values.count{ case Some(_) => true; case None => false}} solutions out of $total")

  persistResults(solutions.toMap, "solutions.ser")

}


