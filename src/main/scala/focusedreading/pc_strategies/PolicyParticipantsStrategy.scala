package focusedreading.pc_strategies

import focusedreading.Participant
import focusedreading.agents.PolicySearchAgent
import focusedreading.models.SearchModel
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.states.{FocusedReadingCompositeState, FocusedReadingState}
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.states.State

import scala.collection.mutable

trait PolicyParticipantsStrategy extends ParticipantChoosingStrategy{


  // Abstract members
  def observeState:State
  def getIterationNum:Int
  val policy:Policy
  var lastActionChosen:Option[Action] = None
  val chosenEndpointsLog = new mutable.ArrayBuffer[((Participant, Participant),(Participant, Participant))]
  ///////////////////


  // Concrete members
  val queryLog = new mutable.ArrayBuffer[(Participant, Participant)]
  val introductions:mutable.HashMap[Participant, Int] = new mutable.HashMap[Participant, Int]()
  val references = new mutable.HashMap[(Participant, Participant, Boolean), Seq[String]]()
  ///////////////////

  // Private auxiliary members
  protected val exploitChooser = new {} with MostRecentParticipantsStrategy {
    override def participantIntroductions: mutable.HashMap[Participant, Int] = introductions
  }

//  protected val exploitChooser = new {} with FurthestParticipantStrategy {}

  protected val exploreChooser = new {} with MostConnectedParticipantsStrategy {}

  //private def possibleActions:Seq[Action] = getUsedActions//Seq(ExploreEndpoints(), ExploitEndpoints())

  private def peekState(a:Participant, b:Participant):FocusedReadingState = {
    this.queryLog += Tuple2(a, b)
    val containsA = introductions.contains(a)
    val containsB = introductions.contains(b)

    if (!containsA)
      introductions += a -> getIterationNum
    if (!containsB)
      introductions += b -> getIterationNum

    val state = this.observeState.asInstanceOf[FocusedReadingState]

    // Undo the changes
    queryLog.remove(queryLog.size - 1)
    if (!containsA)
      introductions.remove(a)
    if (!containsB)
      introductions.remove(b)

    state
  }
  ///////////////////////////

  // Alternate state observation methods
  def observeExploreState(source: Participant, destination: Participant,
                          previouslyChosen: Set[(Participant, Participant)]
                          , model: SearchModel):((Participant, Participant), FocusedReadingState) =
    observeStrategyState(exploreChooser, source, destination, previouslyChosen, model)

  def observeExploitState(source: Participant, destination: Participant,
                          previouslyChosen: Set[(Participant, Participant)]
                          , model: SearchModel):((Participant, Participant), FocusedReadingState) =
    observeStrategyState(exploitChooser, source, destination, previouslyChosen, model)

  private def observeStrategyState(chooser:ParticipantChoosingStrategy,
                                   source: Participant, destination: Participant,
                                   previouslyChosen: Set[(Participant, Participant)],
                                   model: SearchModel):((Participant, Participant), FocusedReadingState) = {
    val endpoints = chooser.choseEndPoints(source, destination, previouslyChosen, model)

    var a = endpoints._1
    var b = endpoints._2

    val state = peekState(a, b)

    (endpoints, state)
  }
  /////////////////////////////////////

  // Strategy implementation
  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)]
                              , model: SearchModel): (Participant, Participant) = {

    // Endpoint choices


    // State variations
    // Explore state
    val (exploreEndpoints, exploreState) = observeExploreState(source, destination, previouslyChosen, model)


    // Exploit state
    val (exploitEndpoints, exploitState) = observeExploitState(source, destination, previouslyChosen, model)
    //////////////////////

    // DEBUG: Keep track of how many times the explore/exploit pairs are equal
    chosenEndpointsLog += Tuple2(exploreEndpoints, exploitEndpoints)


//    val states = PolicySearchAgent.getActiveEndpointActions map {
//      case _:ExploreEndpoints =>
//         "explore" -> exploreState
//      case _:ExploitEndpoints =>
//        "exploit" -> exploitState
//    } toMap

    // Combine both states into a single one
//    val combinedState = FocusedReadingCompositeState(states("explore"), states("exploit"))


    // Choose the action
//    val action = policy.selectAction(combinedState, PolicySearchAgent.getActiveEndpointActions.toSeq)
  val action = policy.selectAction(observeState, PolicySearchAgent.usedActions)

    lastActionChosen = Some(action)

    val chosenEndpoints = action match {
      case ac if Seq(ExploreEndpoints_ExploitQuery(), ExploreEndpoints_ExploreFewQuery(), ExploreEndpoints_ExploreManyQuery()).contains(ac) =>
        exploreEndpoints
      case ac if Seq(ExploitEndpoints_ExploitQuery(), ExploitEndpoints_ExploreFewQuery(), ExploitEndpoints_ExploreManyQuery()).contains(ac) =>
        exploitEndpoints
    }

    // Persist the changes to the state
    this.queryLog += chosenEndpoints

    val a = chosenEndpoints._1
    val b = chosenEndpoints._2

    if(!introductions.contains(a))
      introductions += a -> getIterationNum
    if(!introductions.contains(b))
      introductions += b -> getIterationNum
    //////////////////////////////////

    // Return the chosen endpoints
    chosenEndpoints
  }
  /////////////////////////
}

