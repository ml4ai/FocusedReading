package focusedreading.supervision.search

import focusedreading.reinforcement_learning.actions.FocusedReadingAction

import scala.collection.mutable

class UniformCostSearch(initialState:FRSearchState, maxCost:Double = Double.PositiveInfinity){

  type GoldDatum = Seq[(String, String, Seq[String])]

  protected val queue:mutable.PriorityQueue[Node] = new mutable.PriorityQueue[Node]()

  def solve():Option[Node] ={

    // Get the initial state
    val root = Node(initialState, 0d, None, None)

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
