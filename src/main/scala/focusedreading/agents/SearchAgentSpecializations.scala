package focusedreading.agents

import java.io.{FileOutputStream, OutputStreamWriter}

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.pc_strategies.{MostConnectedParticipantsStrategy, PolicyParticipantsStrategy}
import focusedreading.ie.{REACHIEStrategy, SQLIteIEStrategy}
import focusedreading.ir.QueryStrategy._
import focusedreading.ir.{LuceneIRStrategy, Query, QueryStrategy, RedisIRStrategy, SQLIRStrategy}
import focusedreading.models._
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.states.{FocusedReadingState, RankBin}
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.states.State
import focusedreading._
import focusedreading.agents._
import focusedreading.agents.FocusedReadingStage._
import scala.collection.JavaConversions._

import scala.collection.mutable

/*
 * Created by enrique on 18/02/17.
 *
 * Mixes traits together to implement FR search agents
 */

/**
  * Uses Lucene and REACH directly to do a FR simple path search
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  */
class LuceneReachSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with LuceneIRStrategy
  with REACHIEStrategy {

  // Graph4Scala model
  override val model:SearchModel = new GFSModel(participantA, participantB) // Directed graph with the model.


  // Follow the cascade query strategy
  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, 50, source, Some(destination)) //TODO: Fix this to a more elegant way of ignoring the retrieval count


}

/**
  * Uses Redis for IR and SQLite for IE to do simple path FR
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  */
class RedisSQLiteSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with RedisIRStrategy
  with SQLIteIEStrategy {


  override val model:SearchModel = new GFSModel(participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, 50, source, Some(destination)) //TODO: Fix this to a more elegant way of ignoring the retrieval count

}


/**
  * Companion object. TODO: Find a better place for these fields
  */
object PolicySearchAgent{
  // All the possible actions
  val usedActions = Seq(ExploitQuery(), ExploreManyQuery(), ExploreFewQuery(), ExploitEndpoints(), ExploreEndpoints())
  val config = ConfigFactory.load()
  val elements = config.getConfig("MDP").getConfig("actions").getStringList("active").toSet

  def getActiveActions:Set[Action] = getActiveEndpointActions ++ getActiveQueryActions

  def getActiveEndpointActions:Set[Action] = {

    val activeActions:Set[Action] = elements.collect{
      case "ExploitEndpoints" => ExploitEndpoints()
      case "ExploreEndpoints" => ExploreEndpoints()
    }

    activeActions
  }

  def getActiveQueryActions:Set[Action] = {

    val activeActions:Set[Action] = elements.collect{
      case "ExploitQuery" => ExploitQuery()
      case "ExploreManyQuery" => ExploreManyQuery()
      case "ExploreFewQuery" => ExploreFewQuery()
    }

    activeActions
  }
}


/**
  * Search agent that follows a policy, presumably learnt using RL.
  * Look at the traits to see which strategies it follows
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  * @param policy
  */
class PolicySearchAgent(participantA:Participant, participantB:Participant, val policy:Policy) extends SimplePathAgent(participantA, participantB)
  with PolicyParticipantsStrategy
  with RedisIRStrategy
  with SQLIteIEStrategy {



  // Fields

  val actionCounters = new mutable.HashMap[String, Int]() ++ PolicySearchAgent.usedActions.map(_.toString -> 0).toMap


  var stage:FocusedReadingStage.Value = FocusedReadingStage.EndPoints

  this.introductions += participantA -> 0
  this.introductions += participantB -> 0

  val configuration = ConfigFactory.load()
  val fewPapers:Int = configuration.getConfig("MDP").getConfig("paperAmounts").getInt("few")
  val manyPapers:Int = configuration.getConfig("MDP").getConfig("paperAmounts").getInt("many")
  ////////////

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)],
                              model: SearchModel): (Participant, Participant) = {

    // Choose the endpoints with the policy
    val endpoints = super.choseEndPoints(source, destination, previouslyChosen, model)

    // Keep track of the chosen actions
    actionCounters(this.lastActionChosen.get.toString) += 1

    // Set the stage to query after choosing the endpoints
    stage = FocusedReadingStage.Query

    endpoints
  }

  override val model:SearchModel = new GFSModel(participantA, participantB) // Directed graph with the model.

  override def reconcile(connections: Iterable[Connection]): Unit = {
    // Count the introductions
    for(f <- connections){
      val x = f.controller
      val y = f.controlled

      val sign = f.sign

      // Store the references
      references += (x, y, sign) -> f.reference


      if(!introductions.contains(x))
        introductions += (x -> iterationNum)

      if(!introductions.contains(y))
        introductions += (y -> iterationNum)


    }

    super.reconcile(connections)
  }

  override def successStopCondition(source: Participant, destination: Participant, model: SearchModel): Option[Seq[Seq[Connection]]] = {
    val result = super.successStopCondition(source, destination, model)

    result match {
      case Some(paths) => {
          val newPaths = paths.map {
              path =>
                path.map {
                    connection =>
                      val references = this.references(connection.controller, connection.controlled, connection.sign)
                      Connection(connection.controller, connection.controlled, connection.sign, connection.evidence, references)
                  }

          }

            Some(newPaths)
    }
      case None => None
    }
  }

// TODO: Deal with this correctly. In training, the agent shouldn't quit in the endpoints stage. In testing, it will behave like a search agent, just as the baseline
//  override def failureStopCondition(source: Participant,
//                                    destination: Participant,
//                                    model: SearchModel):Boolean = {
//    // Need to add this condition because I need to account for the two different
//    stage match {
//      case FocusedReadingStage.EndPoints => false
//      case FocusedReadingStage.Query =>{
//        // TODO: Parameterize these numbers into a configuration file
//        if(this.iterationNum >= 200)
//          true
//        else if(iterationNum > 1 && (nodesCount, edgesCount) == (prevNodesCount, prevEdgesCount)){
//          // If the model didn't change, increase the unchanged iterations counter
//          unchangedIterations += 1
//          // This line prints twice because in the policy search agent specialization, the process is divided in two stages:
//          // Endpoints and Query. Each stage takes an iteration
//          // TODO: Fix this by overriding this method in PolicySearchAgent
//          logger.info(s"The model didn't change $unchangedIterations times")
//          if(unchangedIterations >= 10)
//            true
//          else
//            false
//        }
//        else {
//          // Reset the counter of unchanged iterations
//          unchangedIterations = 0
//          false
//        }
//      }
//    }
//
//  }


  override def choseQuery(a: Participant,
                          b: Participant,
                          model: SearchModel): Query = {

    queryLog += Tuple2(a, b)

    val possibleActions:Seq[Action] = PolicySearchAgent.getActiveQueryActions.toSeq

    // Create state
    val state = this.observeState

    // Query the policy
    val (_, action) = policy.selectAction(state, possibleActions)

    // Keep track of the action selection
    actionCounters(action.toString) += 1

    // Set the process stage to endpoint
    stage = FocusedReadingStage.EndPoints

    queryActionToStrategy(action, a, b)
  }

  override def observeState:State = {

    // TODO: Parameterize these to a config file
    val few = 5
    val many = 50

    // Do IR queries
    val (a, b) = queryLog.last
    val exploreFewQuery = Query(QueryStrategy.Disjunction, few, a, Some(b))
    val exploreManyQuery = Query(QueryStrategy.Disjunction, many, a, Some(b))
    val exploitQuery = Query(QueryStrategy.Conjunction, few, a, Some(b))

    val exploreFewIRScores = this.informationRetrival(exploreFewQuery) map (_._2)
    val exploreManyIRScores = this.informationRetrival(exploreFewQuery) map (_._2)
    val exploitIRScores = this.informationRetrival(exploitQuery) map (_._2)

    fillState(this.model, iterationNum, queryLog, introductions, exploreFewIRScores.toSeq, exploreManyIRScores.toSeq, exploitIRScores.toSeq)
  }

  override def getIterationNum: Int = iterationNum

  // Auxiliary methods
  private def fillState(model:SearchModel, iterationNum:Int,
                        queryLog:Seq[(Participant, Participant)],
                        introductions:mutable.Map[Participant, Int],
                        explorationFewIRScores:Seq[Float],
                        explorationManyIRScores:Seq[Float],
                        exploitationIRScores:Seq[Float]):State = {

    val (a, b) = queryLog.last
    val log = queryLog flatMap (l => Seq(l._1, l._2))
    val paQueryLogCount = log.count(p => p == a)
    val pbQueryLogCount = log.count(p => p == b)

    val compA = model.getConnectedComponentOf(a).get
    val compB = model.getConnectedComponentOf(b).get

    val sameComponent = compA == compB

    val paIntro = introductions(a)
    val pbIntro = introductions(b)

    val ranks:Map[Participant, Int] = model.rankedNodes

    val paRank = (ranks(a)+1) / model.numNodes.toDouble //getRank(a, ranks)
    val pbRank = (ranks(b)+1) / model.numNodes.toDouble //getRank(b, ranks)

    val paUngrounded = a.id.toUpperCase.startsWith("UAZ")
    val pbUngrounded = b.id.toUpperCase.startsWith("UAZ")

    assert(paRank >= 0 && paRank <= 1, "PA rank is out of bounds")
    assert(pbRank >= 0 && pbRank <= 1, "PA rank is out of bounds")

    FocusedReadingState(paRank, pbRank, iterationNum, paQueryLogCount,
      pbQueryLogCount,sameComponent,paIntro,pbIntro, paUngrounded,
      pbUngrounded, explorationFewIRScores, explorationManyIRScores, exploitationIRScores, unchangedIterations)
  }

  private def getRank(p:Participant, ranks:Map[Participant, Int]):RankBin.Value = {
    val rank = ranks(p)
    if(rank == 0)
      RankBin.First
    else{
      val size = ranks.size
      if(size < 3)
        RankBin.Upper
      else{
        val stride = size/3
        val cutPoints = 1.to(3).map(i => i*stride).reverse

        var ret =RankBin.Bottom

        val bins = Seq(RankBin.Bottom, RankBin.Mid, RankBin.Upper)

        for((point, i) <- cutPoints.zipWithIndex){
          if(rank <= point)
            ret = bins(i)
        }

        ret
      }

    }
  }


  private val uniquePapers = new mutable.HashSet[String]()

  private def executePolicyQueryStage(action:Action, persist:Boolean):Double = {

    // Fetch the chosen participants (endpoints)
    val (a, b) = queryLog.last

    // Build a query object based on the action
    val query = queryActionToStrategy(action, a, b)

    val paperIds = this.informationRetrival(query)

    //this.uniquePapers ++= paperIds

    val findings = this.informationExtraction(paperIds map (p => p._1))

    // Count the introductions
    for(f <- findings){
      val x = f.controller
      val y = f.controlled

      if(persist){
        if(!introductions.contains(x))
          introductions += (x -> iterationNum)

        if(!introductions.contains(y))
          introductions += (y -> iterationNum)
      }

    }

    // Add the stuff to the model
    reconcile(findings)

    // Increment the iteration count
    iterationNum += 1

    // Set the stage to endpoint
    stage = FocusedReadingStage.EndPoints


    // Return the observed reward
    if(!this.hasFinished(participantA, participantB, model, true)){
      // If this episode hasn't finished
      -0.05
    }
    else{
      // If finished successfully
      val uniquePapers = this.papersRead.toSet.size
      successStopCondition(participantA, participantB, model) match{
        case Some(p) =>
          1.0
        case None =>
          -1.0
      }
    }
  }


  private def queryActionToStrategy(action: Action, a: Participant, b: Participant) = {

    action match {
      case _: ExploitQuery =>
        Query(Conjunction, fewPapers, a, Some(b))
      case _: ExploreManyQuery =>
        Query(Disjunction, manyPapers, a, Some(b))
      case _: ExploreFewQuery =>
        Query(Disjunction, manyPapers, a, Some(b))
      case _ =>
        throw new RuntimeException("Got an invalid action type for the query stage")
    }
  }

  private def executePolicyEndpointsStage(action:Action, persist:Boolean):Double = {
    if(persist)
      iterationNum += 1


    val selectedChooser = action match {
      case _:ExploitEndpoints => exploitChooser
      case _:ExploreEndpoints => exploreChooser
      case _ => throw new RuntimeException("Invalid action for the ENDPOINTS stage")
    }

    val (a, b) = selectedChooser.choseEndPoints(participantA, participantB, triedPairs.toSet, model)
    ////////


    if(persist){
      triedPairs += Tuple2(a, b)
      queryLog += Tuple2(a, b)
    }

    stage = FocusedReadingStage.Query

    0.0 //TODO: Tinker with this reward
  }


  // Public methods
  def executePolicy(action:Action, persist:Boolean = true):Double = (stage: @unchecked) match {
    case FocusedReadingStage.Query => executePolicyQueryStage(action, persist)
    case FocusedReadingStage.EndPoints => executePolicyEndpointsStage(action, persist)
  }

  def possibleActions(): Seq[Action] = (stage: @unchecked) match {
    case FocusedReadingStage.EndPoints => PolicySearchAgent.getActiveEndpointActions.toSeq
    case FocusedReadingStage.Query => PolicySearchAgent.getActiveQueryActions.toSeq
  }
  /////////////////


}



