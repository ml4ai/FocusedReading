package focusedreading.supervision.search

import focusedreading.reinforcement_learning.actions.FocusedReadingAction

/**
  * Node in the search tree for different search algorithms. Makes the assumption of the FR problem
  *
  * @param state Current state of the search
  * @param pathCost Total cost incurred to reach the current state
  * @param action Action taken whose outcome is the current state
  * @param parent Parent node in the search tree
  */
case class Node(state:FRSearchState, pathCost:Double, action:Option[FocusedReadingAction], parent:Option[Node]) extends Ordered[Node] {

  /**
    * Compares two nodes based on their path cost
    * @param that
    * @return
    */
  override def compare(that: Node): Int = {
    Math.ceil(pathCost - that.pathCost).toInt
  }

  /**
    * Considers the hash of the search graph as the node's hash
    * @return
    */
  override def hashCode(): Int = state.agent.model.hashCode()

  /**
    * Makes equality comparison base on the hash code of the node
    * @param obj
    * @return
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that:Node => {
      this.hashCode() == that.hashCode()
    }
    case _ => false
  }
}