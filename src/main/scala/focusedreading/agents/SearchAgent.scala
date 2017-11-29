package focusedreading.agents

import com.typesafe.scalalogging.LazyLogging
import focusedreading.pc_strategies.ParticipantChoosingStrategy
import focusedreading.ie.IEStrategy
import focusedreading.ir.{IRStrategy, Query}
import focusedreading.models._
import focusedreading.tracing.IterativeStep
import focusedreading.{Connection, Participant}

import scala.collection.mutable
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph

/**
  * Created by enrique on 18/02/17.
  */

/**
  * Enumeration representing the stage of the FR process in which an agent may be at any given point
  * The two stages are:
  *  - EndPoints: Selects the elements in the search graph used to anchor the IR query
  *  - Query: Performs the IR query to fetch the documents to do IE further on
  */
object FocusedReadingStage extends Enumeration{
  val EndPoints, Query = Value
}


/**
  * Main trait that implements a FR agent. It searches for paths connecting two entities of interests.
  * Search agent mixes three traits implementing its behaivior:
  *  - IRStrategy: Specifies how to perform information retrieval to fetch documents. Examples: Lucene, SQLite, REDIS
  *  - IEStrategy: Specifies how to perform information extraction to extract information. Examples: REACH, SQLite
  *  - ParticipantChoosingStrategy: Specifies how to implement the strategies to chose endpoints to perform IR
  */
trait SearchAgent extends LazyLogging with IRStrategy with IEStrategy with ParticipantChoosingStrategy {

  // This is the KB graph which will be grown iteratively. This is an abstract field and must be implemented on the
  // concrete class inheriting the trait
  val model:SearchModel

  // Counter that keeps track of the iteration number
  var iterationNum = 0

  // Record of the participant pairs used during the search process
  val triedPairs = new mutable.HashSet[(Participant, Participant)]

  // Debug information memory
  val trace = new mutable.ArrayBuffer[IterativeStep]

  // Record of the documents (PMCIDs) read during the process
  val papersRead = new mutable.ArrayBuffer[String]

  // This is the Focused Reading loop. The heart of the algorithm
  def focusedSearch(source:Participant, destination:Participant):Unit ={

    logger.info(s"Starting focused search with end points $source and $destination")

    do{
      // Increment the iteration counter
      iterationNum += 1
      logger.info(s"Iteration #$iterationNum")
      // Select the nodes in the KB graph that will be used for IR
      val (a, b) = choseEndPoints(source, destination, triedPairs.toSet, model)
      // Keep track of those nodes in the record
      triedPairs += Tuple2(a, b)
      // Chose a query strategy given the nodes and the current state of the KB graph
      val query = choseQuery(a, b, this.model)
      logger.info(s"Chosen query: $query")
      // Do IR to select the documents that will be read
      val irResult = informationRetrival(query)
      // Extract the PMCIDs of the IR result. The other element is the IR score of each paper
      val paperIds = irResult map (_._1)
      // Keep track of the papers read during the process
      papersRead ++= paperIds

      if(paperIds.nonEmpty)
        logger.info(s"Found ${paperIds.size} IR matches")
      else
        logger.info(s"Empty query $query")

      // Do information extraction on those papers
      val findings = informationExtraction(paperIds)
      logger.info(s"Found ${findings.size} connections")

      // Keep track of the KB graph before reconciling the information
      // For debugging purposes
      val modelGraphBefore = getStateGraph

      // Add the newest data to the KB graph
      reconcile(findings)

      // Keep track of the KB graph after
      // For debugging purposes
      val modelGraphAfter = getStateGraph

      // Store the step into the trace
      // For debugging purposes
      val step = IterativeStep(iterationNum, modelGraphBefore, modelGraphAfter, (a, b), query.strategy,
        irResult, findings)

      // Add it to the FR trace
      trace += step

    }
    // Repeat the process until it is finished
    while(!hasFinished(source, destination, this.model))
    logger.info(s"Focused search finished after $iterationNum iterations")
  }

  /**
    * Determines whether the FR search has finished, either successfuly or not
    * @param source node in model
    * @param destination node in model
    * @param model KB graph grown during the FR process
    * @return whether the search process has finished
    */
  def hasFinished(source:Participant, destination:Participant, model:SearchModel):Boolean = {
    // If there is a success stop condition, defined below, then stop the process
    if(successStopCondition(source, destination, model).isDefined)
      true
    // If there is a failure stop condition, defined below, then stop the process
    else if(failureStopCondition(source, destination, model))
      true
    // If there's no stop condition at all, don't stop the process
    else
      false
  }

  /**
    * Defines a failing stop condition. This is an abstract method that must be implemented. An example
    * of this condition is no path between source an destination, or N number of paths between source and destination
    * @param source node in model
    * @param destination node in model
    * @param model KB graph grown during the search process
    * @return Some paths represented as a sequence of connections or None if it doesn't exist
    */
  def successStopCondition(source:Participant,
                           destination:Participant,
                           model:SearchModel):Option[Seq[Seq[Connection]]]

  /**
    * Defines a failing stop condition. This is an abstract method that must be implemented. An example
    * of this condition is that the KB graph didn't change for a number of iterations or if the process exceeded
    * a certain amount of iterations
    * @param source node in model
    * @param destination node in model
    * @param model KB graph grown during the search process
    * @return Whether the FR process has failed
    */
  def failureStopCondition(source:Participant,
                           destination:Participant,
                           model:SearchModel):Boolean


  // TODO: Write docstring for this method
  def choseQuery(source:Participant, destination:Participant, model:SearchModel):Query

  /**
    * Incorporate the machine reading findings into the graph. This is an abstract method that must be implemented.
    * An example is to include the connections into the KB graph if they pass a frequency threshold
    * @param findings Collection of connections to be considered to be added to the KB graph
    */
  def reconcile(findings:Iterable[Connection]):Unit

  /**
    * Fetch the underlying graph of the KB. This is for debugging purposes
    * @return The graph structure of te KB
    */
  protected def getStateGraph: Option[Graph[Participant, LDiEdge]] = this.model match {
    case gsfModel:GFSModel => Some(gsfModel.G.clone())
    case _ => None
  }
}


/**
  * Specialization of the SearchAgent trait that looks for at least one path between PA and PB
  * This is an abstract class because it takes constructor parameters
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  */
abstract class SimplePathAgent(participantA:Participant, participantB:Participant) extends SearchAgent {

  // We use a Graph4Scala implementation of the KB search graph
  val model:SearchModel = new GFSModel(participantA, participantB)

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
    * @return Whether the FR process has failed
    */
  override def failureStopCondition(source: Participant,
                                    destination: Participant,
                                    model: SearchModel):Boolean = {
    // TODO: Parameterize these numbers into a configuration file
    if(this.iterationNum >= 100)
      true
    else if(iterationNum > 1 && (nodesCount, edgesCount) == (prevNodesCount, prevEdgesCount)){
      // If the model didn't change, increase the unchanged iterations counter
      unchangedIterations += 1
      // This line prints twice because in the policy search agent specialization, the process is divided in two stages:
      // Endpoints and Query. Each stage takes an iteration
      // TODO: Fix this by overriding this method in PolicySearchAgent
      logger.info(s"The model didn't change $unchangedIterations times")
      if(unchangedIterations >= 10)
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


    logger.info(s"Model participants; Before: $prevNodesCount\tAfter: $nodesCount")
    logger.info(s"Model connections; Before: $prevEdgesCount\tAfter: $edgesCount")
  }

}