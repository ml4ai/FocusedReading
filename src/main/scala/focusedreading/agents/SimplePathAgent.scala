package focusedreading.agents

import focusedreading.entities.{Connection, Participant}
import focusedreading.search_models.{EfficientSearchModel, SearchModel}

/**
  * Specialization of the SearchAgent trait that looks for at least one path between PA and PB
  * This is an abstract class because it takes constructor parameters
  *
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  */
abstract class SimplePathAgent(participantA:Participant, participantB:Participant) extends SearchAgent {

  // We use a Graph4Scala implementation of the KB search graph
  var model:SearchModel = new EfficientSearchModel(participantA, participantB)//new EfficientSearchModel(participantA, participantB)//

  var (nodesCount, edgesCount) = (0, 0)
  var (prevNodesCount, prevEdgesCount) = (0, 0)
  var unchangedIterations = 0

  /**
    * Succeeds when at least one path is present betwen source and destination. If it exists, we can find it efficiently
    * with a shortest path search
    *
    * @param source node in model
    * @param destination node in model
    * @param model KB graph grown during the search process
    * @return Some paths represented as a sequence of connections or None if it doesn't exist
    */
  override def successStopCondition(source: Participant,
                                    destination: Participant,
                                    model: SearchModel):Option[Seq[Seq[Connection]]] = {
    model.shortestPath(source, destination) match {
      case Some(path) => Some(Seq(path))
      case None => None
    }
  }

  /**
    * Fails if we spent more than N iterations or if the KB didn't change for one iteration
    * @param source node in model
    * @param destination node in model
    * @param model KB graph grown during the search process
    * @param persist Whether to tick up the iteration number
    * @return Whether the FR process has failed
    */
  override def failureStopCondition(source: Participant,
                                    destination: Participant,
                                    model: SearchModel,
                                    persist: Boolean):Boolean = {
    if(this.iterationNum >= maxIterations)
      true
    else if(iterationNum > 1 && (nodesCount, edgesCount) == (prevNodesCount, prevEdgesCount)){
      // If the model didn't change, increase the unchanged iterations counter
      if(persist) {
        unchangedIterations += 1

        //logger.info(s"The model didn't change $unchangedIterations times")
      }
      if(unchangedIterations >= maxUnchangedIterations)
        true
      else
        false
    }
    else {
      // Reset the counter of unchanged iterations
      unchangedIterations = 0
      false
    }
  }

  /**
    * Extend the KB graph with the newest found interactions
    *
    * @param connections New information to incorporate to the graph
    */
  override def reconcile(connections: Iterable[Connection]){
    // How large was the graph before?
    this.prevNodesCount = this.model.numNodes
    this.prevEdgesCount = this.model.numEdges
    // Make labeled directed edges out of each connection
    // Add them to the graph
    this.model addEdges connections
    // How large is it now?
    this.nodesCount = this.model.numNodes
    this.edgesCount = this.model.numEdges


    //logger.info(s"Model participants; Before: $prevNodesCount\tAfter: $nodesCount")
    //logger.info(s"Model connections; Before: $prevEdgesCount\tAfter: $edgesCount")
  }

}
