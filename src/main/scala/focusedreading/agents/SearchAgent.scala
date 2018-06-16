package focusedreading.agents

import com.typesafe.scalalogging.LazyLogging
import focusedreading.entities.{Connection, Participant}
import focusedreading.pc_strategies.ParticipantChoosingStrategy
import focusedreading.ie.IEStrategy
import focusedreading.ir.IRStrategy
import focusedreading.ir.queries.Query
import focusedreading.search_models._
import focusedreading.{Configuration}

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph

/**
  * Created by enrique on 18/02/17.
  */


/**
  * Main trait that implements a FR agent. It searches for paths connecting two entities of interests.
  * Search agent mixes three traits implementing its behaivior:
  *  - IRStrategy: Specifies how to perform information retrieval to fetch documents. Examples: Lucene, SQLite, REDIS
  *  - IEStrategy: Specifies how to perform information extraction to extract information. Examples: REACH, SQLite
  *  - ParticipantChoosingStrategy: Specifies how to implement the strategies to chose endpoints to perform IR
  */
trait SearchAgent extends LazyLogging with IRStrategy with IEStrategy with ParticipantChoosingStrategy {

  val maxIterations = Configuration.MDP.maxIterations
  val maxUnchangedIterations = Configuration.MDP.maxUnchangedIterations

  // This is the KB graph which will be grown iteratively. This is an abstract field and must be implemented on the
  // concrete class inheriting the trait
  var model:SearchModel

  // Counter that keeps track of the iteration number
  var iterationNum = 0

  // Record of the participant pairs used during the search process
  var triedPairs = HashSet[(Participant, Participant)]()

  // Debug information memory
  //val trace = new mutable.ArrayBuffer[IterativeStep]

  // Record of the documents (PMCIDs) read during the process
  var papersRead = mutable.TreeSet[String]()

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
      val irResult = informationRetrieval(query)
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
      //val modelGraphBefore = getStateGraph

      // Add the newest data to the KB graph
      reconcile(findings)

      // Keep track of the KB graph after
      // For debugging purposes
      //val modelGraphAfter = getStateGraph

      // Store the step into the trace
      // For debugging purposes
      //val step = IterativeStep(iterationNum, modelGraphBefore, modelGraphAfter, (a, b), query.strategy,
      //  irResult, findings)

      // Add it to the FR trace
      //trace += step

    }
    // Repeat the process until it is finished
    while(!hasFinished(source, destination, this.model, true))
    logger.info(s"Focused search finished after $iterationNum iterations")
  }

  /**
    * Determines whether the FR search has finished, either successfuly or not
    * @param source node in model
    * @param destination node in model
    * @param model KB graph grown during the FR process
    * @return whether the search process has finished
    */
  def hasFinished(source:Participant, destination:Participant, model:SearchModel, mutate:Boolean):Boolean = {
    // If there is a success stop condition, defined below, then stop the process
    if(successStopCondition(source, destination, model).isDefined)
      true
    // If there is a failure stop condition, defined below, then stop the process
    else if(failureStopCondition(source, destination, model, mutate))
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
    * @param mutate Whether to tick up the iteration number
    * @return Whether the FR process has failed
    */
  def failureStopCondition(source:Participant,
                           destination:Participant,
                           model:SearchModel,
                           mutate:Boolean):Boolean



  /**
    * Select a query strategy anchored on the parameters
    * @param source Anchor participant
    * @param destination Anchor participant
    * @param model Search graph
    * @return Resulting instance of query strategy
    */
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
//  protected def getStateGraph: Option[Graph[Participant, LDiEdge]] = this.model match {
//    case gsfModel:GFSModel => Some(gsfModel.getGraph.clone())
//    case _ => None
//  }
}


