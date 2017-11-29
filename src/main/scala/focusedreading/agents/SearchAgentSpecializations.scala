package focusedreading.agents

import java.io.{FileOutputStream, OutputStreamWriter}

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
                          model: SearchModel) = Query(Cascade, source, Some(destination))


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
                          model: SearchModel) = Query(Cascade, source, Some(destination))

}


/**
  * Companion object. TODO: Find a better place for these fields
  */
object PolicySearchAgent{
  // All the possible actions
  val usedActions = Seq(ExploitQuery(), ExploreQuery(), ExploitEndpoints(), ExploreEndpoints())

  // Set to false the actions that you want to ignore
  val usedQueryActions = usedActions filter {
    case _:ExploreQuery => true
    case _:ExploitQuery => true
    case _ => false
  }

  // Set to false the actions that you want to ignore
  val usedEndpointActions = usedActions filter {
    case _:ExploreEndpoints => true
    case _:ExploitEndpoints => false
    case _ => false
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
  //with LuceneIRStrategy
  with SQLIteIEStrategy {



  // Fields

  val actionCounters = new mutable.HashMap[String, Int]() ++ PolicySearchAgent.usedActions.map(_.toString -> 0).toMap


  var stage:FocusedReadingStage.Value = FocusedReadingStage.EndPoints

  this.introductions += participantA -> 0
  this.introductions += participantB -> 0
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


  override def choseQuery(a: Participant,
                          b: Participant,
                          model: SearchModel) = {

    queryLog += Tuple2(a, b)

    val possibleActions:Seq[Action] = PolicySearchAgent.usedQueryActions//Seq(ExploreQuery(), ExploitQuery())

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
    // Do IR queries
    val (a, b) = queryLog.last
    val exploreQuery = Query(QueryStrategy.Disjunction, a, Some(b))
    val exploitQuery = Query(QueryStrategy.Conjunction, a, Some(b))

    val exploreIRScores = this.informationRetrival(exploreQuery) map (_._2)
    val exploitIRScores = this.informationRetrival(exploitQuery) map (_._2)

    // Aggregate IR scores

    val meanExploreScore = if(exploreIRScores.size == 0) 0.0 else (exploreIRScores.sum / exploreIRScores.size)
    val meanExploitScore = if(exploitIRScores.size == 0) 0.0 else (exploitIRScores.sum / exploitIRScores.size)

    fillState(this.model, iterationNum, queryLog, introductions, meanExploreScore, meanExploitScore)
  }

  override def getIterationNum: Int = iterationNum

  override def getUsedActions: Seq[Action] = PolicySearchAgent.usedEndpointActions

  // Auxiliary methods
  private def fillState(model:SearchModel, iterationNum:Int,
                        queryLog:Seq[(Participant, Participant)],
                        introductions:mutable.Map[Participant, Int],
                        explorationIRScore:Double, exploitationIRScore:Double):State = {

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
      pbUngrounded, explorationIRScore, exploitationIRScore)
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
    if(!this.hasFinished(participantA, participantB, model)){
      // If this episode hasn't finished
      -0.05
    }
    else{
      // If finished successfuly
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
        Query(Conjunction, a, Some(b))
      case _: ExploreQuery =>
        Query(Disjunction, a, Some(b))
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
    case FocusedReadingStage.EndPoints => PolicySearchAgent.usedEndpointActions
    case FocusedReadingStage.Query => PolicySearchAgent.usedQueryActions
  }
  /////////////////


}


