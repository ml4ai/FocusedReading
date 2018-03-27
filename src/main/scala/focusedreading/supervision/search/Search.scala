package focusedreading.supervision.search

import java.io
import java.io.{FileOutputStream, ObjectOutputStream, OutputStreamWriter}

import com.typesafe.config.ConfigFactory
import focusedreading.Participant
import focusedreading.agents.PolicySearchAgent
import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.supervision.CreateExpertOracle
import focusedreading.supervision.search.Search.GoldDatum

import scala.collection.mutable
import scala.collection.parallel.ForkJoinTaskSupport


object Search extends App{

//  val support = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(4))

  private val configuration = ConfigFactory.load()
  val maxIterations = configuration.getConfig("MDP").getInt("maxIterations")

  type GoldDatum = Seq[(String, String, Seq[String])]


  val groundTruth: Map[(String, String), Option[GoldDatum]] = CreateExpertOracle.deserialize("shortest_paths.ser")

  val solutions = new mutable.HashMap[(String, String), Option[Seq[FocusedReadingAction]]]()

  val total = groundTruth.size

  val start = System.currentTimeMillis()

  val collection = groundTruth.keys.toSeq.zipWithIndex
//
//  collection.tasksupport = support


  for((k, ix) <- collection){

    println(s"${ix+1} out of $total.\t$k")

    val path = groundTruth(k).get

    val (participantA, participantB) = (new Participant("", path.head._1), new Participant("", path.last._2))

    val agent = new PolicySearchAgent(participantA, participantB)

    val initialState = FRSearchState(agent, path, 0)

    val problem = FocusedReadingSearchProblem(initialState)

    val solver = new UniformCostSearch(problem)

    val result = solver.solve()

    solutions(k) = result match {
      case Some(r) => Some(solver.actionSequence(r))
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
    val failedState = depth > Search.maxIterations//agent.failureStopCondition(agent.participantA, agent.participantB, agent.model, persist = false)

    if(failedState){
      Double.PositiveInfinity
    }
    else{
      agent.uniquePapers.size //  TODO: Keep an eye on this, maybe change it for a better cost function, consider the action length
    }
  }

  def updateState():Unit = {
    val pending = steps dropWhile stepsDiscovered

    pending foreach {
      k =>
        val path = agent.model.shortestPath(Participant("", k._1), Participant("", k._2))
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

case class FocusedReadingSearchProblem(initialState:FRSearchState)

class UniformCostSearch(problem:FocusedReadingSearchProblem){

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

  def solve():Option[Node] ={

    // Get the initial state
    val root = Node(problem.initialState, 0d, None, None)

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
          if(child.pathCost != Double.PositiveInfinity){
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
