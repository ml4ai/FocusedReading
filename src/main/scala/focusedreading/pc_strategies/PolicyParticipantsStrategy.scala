package focusedreading.pc_strategies

import focusedreading.Participant
import focusedreading.agents.PolicySearchAgent
import focusedreading.models.SearchModel
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.states.FocusedReadingState
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.states.State
import focusedreading.implicits._

import scala.collection.mutable

trait PolicyParticipantsStrategy extends ParticipantChoosingStrategy{


  // Abstract members
  def observeState:State
  def getIterationNum:Int
  val policy:Option[Policy]
  ///////////////////


  // Concrete members
  var lastActionChosen:Option[Action] = None
  var chosenEndpointsLog = List[((Participant, Participant),(Participant, Participant))]()
  var queryLog = List[(Participant, Participant)]()
  var introductions:Map[Participant, Int] = Map[Participant, Int]()
  var references = Map[(Participant, Participant, Boolean), Seq[String]]()
  ///////////////////

  // Private auxiliary members
  protected val exploitChooser = new {} with MostRecentParticipantsStrategy {
    override def participantIntroductions: Map[Participant, Int] = introductions
  }

//  protected val exploitChooser = new {} with FurthestParticipantStrategy {}

  protected val exploreChooser = new {} with MostConnectedParticipantsStrategy {}

  //private def possibleActions:Seq[Action] = getUsedActions//Seq(ExploreEndpoints(), ExploitEndpoints())

  private def peekState(a:Participant, b:Participant):FocusedReadingState = {
    this.queryLog = (a, b)::this.queryLog
    val containsA = introductions.contains(a)
    val containsB = introductions.contains(b)

    if (!containsA)
      introductions += a -> getIterationNum
    if (!containsB)
      introductions += b -> getIterationNum

    val state:FocusedReadingState = this.observeState

    // Undo the changes
    //queryLog.remove(queryLog.size - 1)
    queryLog = queryLog.dropRight(1)
    if (!containsA)
      introductions -= a
    if (!containsB)
      introductions -= b

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
                              , model: SearchModel): (Participant, Participant) =  policy match {

    case Some(p) => {
      // Endpoint choices


      // State variations
      // Explore state
      val (exploreEndpoints, exploreState) = observeExploreState(source, destination, previouslyChosen, model)


      // Exploit state
      val (exploitEndpoints, exploitState) = observeExploitState(source, destination, previouslyChosen, model)
      //////////////////////

      // DEBUG: Keep track of how many times the explore/exploit pairs are equal
      chosenEndpointsLog = (exploreEndpoints, exploitEndpoints)::chosenEndpointsLog


      // Choose the action
      val action = p.selectAction(observeState, PolicySearchAgent.usedActions)

      lastActionChosen = Some(action)

      val chosenEndpoints = action match {
        case ac if Seq(ExploreEndpoints_ExploitQuery, ExploreEndpoints_ExploreFewQuery, ExploreEndpoints_ExploreManyQuery).contains(ac) =>
          exploreEndpoints
        case ac if Seq(ExploitEndpoints_ExploitQuery, ExploitEndpoints_ExploreFewQuery, ExploitEndpoints_ExploreManyQuery).contains(ac) =>
          exploitEndpoints
      }

      // Persist the changes to the state
      queryLog = chosenEndpoints::queryLog

      val a = chosenEndpoints._1
      val b = chosenEndpoints._2

      if (!introductions.contains(a))
        introductions += a -> getIterationNum
      if (!introductions.contains(b))
        introductions += b -> getIterationNum
      //////////////////////////////////

      // Return the chosen endpoints
      chosenEndpoints
    }
    case None => throw new IllegalStateException("The agent wasn't provided with a policy")
  }
  /////////////////////////
}

