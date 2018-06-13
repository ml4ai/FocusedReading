package focusedreading.agents

import com.typesafe.config.ConfigFactory
import focusedreading.{Connection, Participant}
import focusedreading.ie.SQLIteIEStrategy
import focusedreading.ir.QueryStrategy.{Conjunction, Disjunction}
import focusedreading.ir.{Query, QueryStrategy, RedisIRStrategy, SQLIRStrategy}
import focusedreading.models.{EfficientSearchModel, GFSModel, SearchModel}
import focusedreading.pc_strategies.PolicyParticipantsStrategy
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.states.{FocusedReadingState, NormalizationParameters, RankBin}
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.states.State

import scala.collection.JavaConversions._
import scala.collection.mutable


/**
  * Search agent that follows a policy, presumably learnt using RL.
  * Look at the traits to see which strategies it follows
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  * @param policy Policy to follow
  * @param referencePath Expert path to compute the reward shaping potential
  */
class PolicySearchAgent(val participantA:Participant, val participantB:Participant,
                        val policy:Option[Policy] = None,
                        val referencePath:Option[Seq[Participant]] = None,
                        val normalizationParameters:Option[NormalizationParameters] = None)(implicit indexPath:LuceneIndexDir, sqliteFile: SQLiteFile) extends SimplePathAgent(participantA, participantB) with Serializable
  with PolicyParticipantsStrategy
  with RedisIRStrategy
  with SQLIteIEStrategy {


  override def clone():PolicySearchAgent = {
    val clone = new PolicySearchAgent(participantA, participantB, policy, referencePath, normalizationParameters)(this.indexPath, this.sqliteFile)

    clone.model = this.model.copy()
    clone.actionCounters ++= this.actionCounters
    clone.nodesCount = this.nodesCount
    clone.edgesCount = this.edgesCount
    clone.prevNodesCount = this.prevNodesCount
    clone.prevEdgesCount = this.prevEdgesCount
    clone.unchangedIterations = this.unchangedIterations

    clone.iterationNum = clone.iterationNum
    clone.triedPairs = this.triedPairs
    clone.papersRead = this.papersRead
    //clone.trace ++= this.trace

    clone.references = this.references
    clone.queryLog = this.queryLog
    clone.introductions = this.introductions
    clone.lastActionChosen = this.lastActionChosen
    clone.chosenEndpointsLog = this.chosenEndpointsLog
    clone.colors = this.colors

    clone
  }

  override def indexDir: String = indexPath.path
  override def sqlitePath: String = sqliteFile.path

  // Fields
  val actionCounters: mutable.Map[String, Int] = new mutable.HashMap[String, Int]() ++ PolicySearchAgent.usedActions.map(_.toString -> 0).toMap


  this.introductions += participantA -> 0
  this.introductions += participantB -> 0

  private val useRewardShaping = PolicySearchAgent.configuration.getConfig("MDP").getBoolean("rewardShaping")
  private val fewPapers = PolicySearchAgent.configuration.getConfig("MDP").getConfig("paperAmounts").getInt("few")
  private val manyPapers = PolicySearchAgent.configuration.getConfig("MDP").getConfig("paperAmounts").getInt("many")

  var shapingCount:Int = 0
  var rewardEvaluated:Int = 0
  ////////////

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)],
                              model: SearchModel): (Participant, Participant) = {

    // Choose the endpoints with the policy
    val endpoints = super.choseEndPoints(source, destination, previouslyChosen, model)

    // Keep track of the chosen actions
    actionCounters(this.lastActionChosen.get.toString) += 1


    endpoints
  }

  /*override var*/ model/*:SearchModel*/ = new EfficientSearchModel(participantA, participantB) // Directed graph with the model.

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

//  override def successStopCondition(source: Participant, destination: Participant, model: SearchModel): Option[Seq[Seq[Connection]]] = {
//    val result = super.successStopCondition(source, destination, model)
//
//    result match {
//      case Some(paths) => {
//        val newPaths = paths.map {
//          path =>
//            path.map {
//              connection =>
//                //val references = this.references(connection.controller, connection.controlled, connection.sign)
//                // TODO: Possible leak here
//                Connection.get(connection.controller, connection.controlled, connection.sign/*, connection.evidence, references*/)
//            }
//
//        }
//
//        Some(newPaths)
//      }
//      case None => None
//    }
//  }


  override def choseQuery(a: Participant,
                          b: Participant,
                          model: SearchModel): Query = policy match {

    case Some(p) => {
      queryLog = (a, b)::queryLog

      val possibleActions: Seq[Action] = PolicySearchAgent.usedActions

      // Create state
      val state = this.observeState

      // Query the policy
      val action = p.selectAction(state, possibleActions)

      // Keep track of the action selection
      actionCounters(action.toString) += 1


      queryActionToStrategy(action, a, b)
    }
    case None => throw new IllegalStateException("This agent wasn't provided with a policy")
  }

  override def observeState:State = {
    fillState(this.model, iterationNum, queryLog, introductions)
  }

  override def getIterationNum: Int = iterationNum

  // Auxiliary methods
  private def fillState(model:SearchModel, iterationNum:Int,
                        queryLog:Seq[(Participant, Participant)],
                        introductions:Map[Participant, Int]):State = {

    val searchGraphElements = this.model.edges.map{
      conn =>
        (conn.controller.id, conn.controlled.id, conn.sign)
    }.toSet

    if(queryLog.nonEmpty) {



      val (a, b) = queryLog.last
      val log = queryLog flatMap (l => Seq(l._1, l._2))
      val paQueryLogCount = log.count(p => p == a)
      val pbQueryLogCount = log.count(p => p == b)

      val few = this.fewPapers
      val many = this.manyPapers

      val exploreFewQuery = Query(QueryStrategy.Disjunction, few, a, Some(b))
      val exploreManyQuery = Query(QueryStrategy.Disjunction, many, a, Some(b))
      val exploitQuery = Query(QueryStrategy.Conjunction, few, a, Some(b))

      val exploreFewIRScores = this.informationRetrival(exploreFewQuery) map (_._2) toSeq
      val exploreManyIRScores = this.informationRetrival(exploreManyQuery) map (_._2) toSeq
      val exploitIRScores = this.informationRetrival(exploitQuery) map (_._2) toSeq


      val compA = model.getConnectedComponentOf(a).get
      val compB = model.getConnectedComponentOf(b).get

      val sameComponent = compA == compB

      val paIntro = introductions(a)
      val pbIntro = introductions(b)

      val ranks: Map[Participant, Int] = model.rankedNodes

      val paRank = (ranks(a) + 1) / model.numNodes.toDouble //getRank(a, ranks)
      val pbRank = (ranks(b) + 1) / model.numNodes.toDouble //getRank(b, ranks)

      val paUngrounded = a.id.toUpperCase.startsWith("UAZ")
      val pbUngrounded = b.id.toUpperCase.startsWith("UAZ")

      if(paRank < 0 || paRank > 1) println("PA rank is out of bounds")
      if(pbRank < 0 || pbRank > 1) println("PB rank is out of bounds")

      FocusedReadingState(participantA.id, participantB.id, searchGraphElements, paRank, pbRank, iterationNum, paQueryLogCount,
        pbQueryLogCount, sameComponent, paIntro, pbIntro, paUngrounded,
        pbUngrounded, exploreFewIRScores, exploreManyIRScores,
        exploitIRScores, unchangedIterations)
    }
    else{

      FocusedReadingState(participantA.id, participantB.id, searchGraphElements, 0, 0, iterationNum, 0,
        0, false, 0, 0, false,
        false, Seq(0), Seq(0),
        Seq(0), 0)
    }
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


  private def executePolicyQueryStage(action:Action, persist:Boolean):Double = {

    // Compute the reward shaping potential in the current state
    val prevPotential = useRewardShaping match {
      case false => 0.0
      case true => shapingPotential
    }

    // Fetch the chosen participants (endpoints)
    val (a, b) = queryLog.head

    // Build a query object based on the action
    val query = queryActionToStrategy(action, a, b)

    val paperIds = this.informationRetrival(query)

    this.papersRead ++= paperIds map (_._1.intern())

    val findings = this.informationExtraction(paperIds map (p => p._1.intern()))

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



    // Compute the reward shaping, if on
    val currentPotential = if (useRewardShaping) {
      shapingPotential
    } else {
      0.0
    }

    // Reward shaping function (potential difference)
    val rewardShapigCoefficient = mdpConfig.getDouble("rewardShapingCoefficient")
    val shaping = rewardShapigCoefficient*currentPotential - prevPotential


    // TODO: Delete me
    if(shaping > 0) {
      shapingCount += 1
    }

    rewardEvaluated += 1
    /////////////////
    // Return the observed reward
    if(!this.hasFinished(participantA, participantB, model, true)){
      // If this episode hasn't finished
      -0.05 + shaping
    }
    else{
      // If finished successfully
      val uniquePapers = this.papersRead.toSet.size
      successStopCondition(participantA, participantB, model) match{
        case Some(p) =>
          10.0 + shaping
        case None =>
          -1.0 + shaping
      }
    }
  }

  // This is an optimization to avoid running shortest paths every time
  // Refer to https://people.eecs.berkeley.edu/~pabbeel/cs287-fa09/readings/NgHaradaRussell-shaping-ICML1999.pdf
  // For the theoretical justification of this reward shaping set up
  private val shapingCache = new mutable.HashSet[(Participant, Participant)]


  /**
    * Computes the reward shaping potential
    * @return Reward shaping potential for the current state of the search
    */
  private def shapingPotential:Double = {

    referencePath match {

      case Some(path) =>
        // If the path is only a pair, return zero
        if(path.size == 2)
          0.0
        else{
          // Get the hops in the reference path
          val pairs = path.sliding(2).toList // (a, b, c) => (a, b), (b, c)
          var segmentsFound = 0 // Counter of the number of segments in the model
          var flag = true // Shortcut flag to break the loop
          val nodeSet = model.nodes.toSet // Set of nodes currently in the search graph

          // Iterate through every pair of participants in the reference path to find a path among them
          for(pair <- pairs){
            val (a, b) = (pair(0), pair(1))
            // If the previous segment wasn't found, then not continue
            if(flag){
              // Test whether the destination exists in the model
              if(nodeSet.contains(b)){
                // If it exists, see if there's a directed path between the nodes
                // First check the cache
                if(shapingCache.contains((a, b))){
                  segmentsFound +=1
                }
                // Then run shortest path in the model
                else{
                  val segment = model.shortestPath(a, b)
                  // If found, cache it
                  if(segment.isDefined){
                    segmentsFound += 1
                    shapingCache += ((a, b))
                  }
                  // If not, then shortcut the potential
                  else{
                    flag = false
                  }
                }

              }
              else{
                flag = false
              }
            }
          }

          // The potential is the proportions of segments in the reference path found in the model
          segmentsFound / pairs.size
        }

      case None => 0.0
    }
  }

  private def queryActionToStrategy(action: Action, a: Participant, b: Participant) = {

    action match {
      case ac if Seq(ExploitEndpoints_ExploitQuery, ExploreEndpoints_ExploitQuery).contains(ac) =>
        Query(Conjunction, fewPapers, a, Some(b))
      case ac if Seq(ExploitEndpoints_ExploreManyQuery, ExploreEndpoints_ExploreManyQuery).contains(ac) =>
        Query(Disjunction, manyPapers, a, Some(b))
      case ac if Seq(ExploitEndpoints_ExploreFewQuery, ExploreEndpoints_ExploreFewQuery).contains(ac) =>
        Query(Disjunction, manyPapers, a, Some(b))
      case _ =>
        throw new RuntimeException("Got an invalid action type for the query stage")
    }
  }

  private def executePolicyEndpointsStage(action:Action, persist:Boolean):Double = {
    if(persist)
      iterationNum += 1


    val selectedChooser = action match {
      case ac if Seq(ExploitEndpoints_ExploitQuery, ExploitEndpoints_ExploreManyQuery, ExploitEndpoints_ExploreFewQuery).contains(ac) => exploitChooser
      case ac if Seq(ExploreEndpoints_ExploitQuery, ExploreEndpoints_ExploreManyQuery, ExploreEndpoints_ExploreFewQuery).contains(ac) => exploreChooser
      case _ => throw new RuntimeException("Invalid action for the ENDPOINTS stage")
    }

    val (a, b) = selectedChooser.choseEndPoints(participantA, participantB, triedPairs.toSet, model)
    ////////


    if(persist){
      triedPairs += Tuple2(a, b)
      queryLog = (a, b)::queryLog
    }

    0.0 // This reward is zero because this is an intermediate step of the FR loop
    // The actual signal comes after the query stage whether it found a path
  }


  // Public methods
  def executePolicy(action:Action, persist:Boolean = true):Double =  {
    executePolicyEndpointsStage(action, persist)
    /*case FocusedReadingStage.Query => */
    executePolicyQueryStage(action, persist)
    /*case FocusedReadingStage.EndPoints => */

  }

  def possibleActions(): Seq[Action] = PolicySearchAgent.usedActions
  /////////////////


}


/**
  * Companion object.
  */
object PolicySearchAgent {
  val configuration = ConfigFactory.load()
  // All the possible actions
  val usedActions = Seq(ExploitEndpoints_ExploreManyQuery, ExploitEndpoints_ExploreFewQuery, ExploitEndpoints_ExploitQuery,
    ExploreEndpoints_ExploreManyQuery, ExploreEndpoints_ExploreFewQuery, ExploreEndpoints_ExploitQuery)

  //val config = ConfigFactory.load()
  val elements = configuration.getConfig("MDP").getConfig("actions").getStringList("active").toSet

  def getActiveActions: Set[Action] = usedActions.toSet
}

