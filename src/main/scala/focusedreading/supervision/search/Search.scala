package focusedreading.supervision.search

import java.io
import java.io.{FileOutputStream, ObjectOutputStream, OutputStreamWriter}

import com.typesafe.config.{Config, ConfigFactory}
import focusedreading.{Connection, Participant}
import focusedreading.agents.PolicySearchAgent
import focusedreading.ir.LuceneQueries
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.sqlite.SQLiteQueries
import focusedreading.supervision.CreateExpertOracle
import focusedreading.supervision.search.Search.GoldDatum

import scala.collection.mutable
import scala.io
import scala.collection.parallel.ForkJoinTaskSupport
import scala.io.Source


object Search extends App{

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

  def actionSequence(node:Node):List[FocusedReadingAction] = {
    if(node.parent.isDefined){
      if(node.action.isDefined){
        node.action.get :: actionSequence(node.parent.get)
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
  val trainingFile = configuration.getConfig("training").getString("inputFile")

  val trainingPaths = Source.fromFile(trainingFile).getLines().toList.map(_.split("\t")).map(s => s.sliding(2).toList)

  type GoldDatum = Seq[(String, String, Seq[String])]


  val groundTruth: Map[(String, String), Option[GoldDatum]] = CreateExpertOracle.deserialize("shortest_paths.ser")

  val trainingData = trainingPaths.map{
    s =>
      val key = (s.head(0), s.last(1))
      val sequence = s.flatMap{
        p =>
          groundTruth((p(0), p(1))).get
      }

      key -> sequence
  }.toMap

  val solutions = new mutable.HashMap[(String, String), Option[Seq[FocusedReadingAction]]]()

  val total = trainingData.size

  val start = System.currentTimeMillis()

  val collection = trainingData.keys.toSeq.zipWithIndex


  for((k, ix) <- collection){

    println(s"${ix+1} out of $total.\t$k")

    val path = trainingData(k)

    val (participantA, participantB) = (Participant.get("", path.head._1), Participant.get("", path.last._2))

    val agent = new PolicySearchAgent(participantA, participantB)

    val solver = new UniformCostSearch(agent, path)
    //val solver = new IterativeLengtheningSearch(agent, path, stepSize*10, stepSize, stepSize*100)

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

  val osw = new ObjectOutputStream(new FileOutputStream("solutions.ser"))
  osw.writeObject(solutions.toMap)
  osw.close()

}

case class FRSearchState(agent:PolicySearchAgent, groundTruth:GoldDatum, depth:Int){



  def this(agent:PolicySearchAgent, referenceState:FRSearchState) {
    this(agent, referenceState.groundTruth, referenceState.depth+1)
    for(k <- referenceState.stepsDiscovered.keySet){
      stepsDiscovered(k) = referenceState.stepsDiscovered(k)
    }
  }

  private val steps = groundTruth map { case(a, b, _) => (a, b)}
  val stepsDiscovered: mutable.Map[(String, String), Boolean] = new mutable.HashMap[(String, String), Boolean]() ++ (steps map { _ -> false })


  def finished:Boolean = stepsDiscovered.values.count(!_) == 0 // There shouldn't be any false element to be finished

  def cost:Double = {
    val failedState = depth > Search.maxIterations

    if(failedState){
      Double.PositiveInfinity
    }
    else{
      agent.papersRead.size //  TODO: Keep an eye on this, maybe change it for a better cost function, consider the action length
    }
  }

  def updateState():Unit = {
    val pending = steps dropWhile stepsDiscovered

    pending foreach {
      k =>
        val path = agent.model.shortestPath(Participant.get("", k._1), Participant.get("", k._2))
        path match {
          case Some(_) => stepsDiscovered(k) = true
          case None => Unit
        }

    }
  }
}

case class Node(state:FRSearchState, pathCost:Double, action:Option[FocusedReadingAction], parent:Option[Node]) extends Ordered[Node] {
  override def compare(that: Node): Int = {
    Math.ceil(pathCost - that.pathCost).toInt
  }

  override def hashCode() = state.agent.model.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case that:Node => {
      this.hashCode() == that.hashCode()
    }
    case _ => false
  }
}


class UniformCostSearch(agent:PolicySearchAgent, groundTruth:GoldDatum, maxCost:Double = Double.PositiveInfinity){

  val initialState = FRSearchState(agent, groundTruth, 0)

  def solve():Option[Node] ={

    // Get the initial state
    val root = Node(initialState, 0d, None, None)

    val queue = mutable.PriorityQueue(root)

    val explored = new mutable.HashSet[Node]()

    var solution:Option[Node] = None

    while(solution.isEmpty && queue.nonEmpty){
      val node = queue.dequeue()
      explored += node

      val state = node.state
      val success = state.finished

      if(success){
        solution = Some(node)
      }
      else{
        val agent = state.agent

        // Create a node for each possible action at this point
        val children = agent.possibleActions().par map {
          action =>
            val newAgent = agent.clone()
            newAgent.executePolicy(action)
            val newState = new FRSearchState(newAgent, state)
            newState.updateState()
            val childNode = Node(newState, newState.cost, Some(action.asInstanceOf[FocusedReadingAction]), Some(node))
            childNode
        }

        for(child <- children.seq){
          if(child.pathCost < maxCost){
            if(!explored.contains(child)) {
              if(queue.count(n => n == child) == 0)
                queue.enqueue(child)
            }
            else{
              val existing = queue.find(other => child == other)
              existing match {
                case Some(e) =>
                  if(child < e) {
                    val elements = queue.clone.toSeq.filter(_ != e)
                    queue.clear()
                    elements foreach { x => queue.enqueue(x) }
                  }
                case None => Unit
              }
            }
          }
        }
      }
    }

    solution
  }
}

class IterativeLengtheningSearch(agent:PolicySearchAgent, groundTruth:GoldDatum, startingCost:Double,
                                 increment:Double, maxCost:Double){

  def solve():Option[Node] = {
    var solution:Option[Node] = None

    var costBound = startingCost
    do{
      println(s"Doing ILS with cost bound of: $costBound")
      val clone = agent.clone()
      val searcher = new UniformCostSearch(clone, groundTruth, costBound)
      solution = searcher.solve()
      costBound += increment
    }while(solution.isEmpty && costBound <= maxCost)

    solution
  }

}