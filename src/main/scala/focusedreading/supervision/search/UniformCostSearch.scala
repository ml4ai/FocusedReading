package focusedreading.supervision.search

import  focusedreading.implicits._
import focusedreading.reinforcement_learning.actions.FocusedReadingAction

import scala.collection.mutable

class UniformCostSearch(initialState:FRSearchState, maxCost:Double = Double.PositiveInfinity){

  type GoldDatum = Seq[(String, String, Seq[String])]

  protected val queue:mutable.PriorityQueue[Node] = new mutable.PriorityQueue[Node]()

  def estimateRemaining(state:FRSearchState):Int = 0

  protected def createNode(state:FRSearchState, node:Option[Node], action:Option[FocusedReadingAction]):Node = {
    Node(state, /*state.cost.toInt, 0, state.remainingCost,*/ action, node, node match { case Some(n) => n.depth +1; case None => 0})
  }

  def solve():Option[Node] ={

    // Get the initial state
    val root = createNode(initialState, None, None)//Node(initialState, initialState.cost.toInt, 0, initialState.remainingCost, None, None)

    queue.enqueue(root)

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
        val children = agent.possibleActions.par map {
          action =>
            val newAgent = agent.clone()
            newAgent.executePolicy(action)
            val newState = new FRSearchState(newAgent, state)
            newState.updateState()
            val childNode = createNode(newState, Some(node), Some(action))
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
