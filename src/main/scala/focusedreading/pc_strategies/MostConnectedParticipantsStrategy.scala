package focusedreading.pc_strategies

import focusedreading.Participant
import focusedreading.models.SearchModel

import scala.collection.mutable

trait MostConnectedParticipantsStrategy extends ParticipantChoosingStrategy{

  import Color._

  override def choseEndPoints(source:Participant,
                     destination:Participant,
                     previouslyChosen:Set[(Participant, Participant)],
                     model:SearchModel) = {



    // Find the components to which each endpoint belongs
    val components = (model.getConnectedComponentOf(source), model.getConnectedComponentOf(destination))


    // Sorted by degree
    var (sA, sB) = components match {
      case (Some(comS), Some(comD)) =>
        val sortedS = comS.toSeq.map(n => (model.degree(n), n)).sortBy(n => n._1)//.reverse
        val sortedD = comD.toSeq.map(n => (model.degree(n), n)).sortBy(n => n._1)//.reverse
        (sortedS, sortedD)
      case _ => throw new RuntimeException("BEEEP!!")
    }


    val ssA = new mutable.Stack[Participant]()
    ssA.pushAll(sA map (_._2))

    val ssB = new mutable.Stack[Participant]()
    ssB.pushAll(sB map (_._2))

    val allNodes = new mutable.Stack[Participant]()
    allNodes.pushAll(model.nodes.toSeq.sortBy(n => model.degree(n)))//.reverse)

    var endpoints:(Participant, Participant) = (null, null)
    var elegible = true

    do{
      endpoints = pickEndpoints(ssA, ssB)
      // Check the colors of the nodes
      val (ca, cb) = endpoints match {
        case (a, b) => (colors.getOrElse(a, White), colors.getOrElse(b, White))
      }

      elegible = ca != Black && cb != Black


    }while(!elegible && !differentEndpoints(endpoints, previouslyChosen) && ssA.nonEmpty && ssB.nonEmpty)

    // Fallback if there are no new nodes in the components
    if(!differentEndpoints(endpoints, previouslyChosen)){
      ssA.pushAll(sA map (_._2))
      do{
        endpoints = pickEndpoints(ssA, allNodes)
      }while(!differentEndpoints(endpoints, previouslyChosen) && ssA.nonEmpty && allNodes.nonEmpty)
    }


    // Color the chosen nodes
    endpoints match {
      case (a, b) =>
        colors(a) = Black
        colors(b) = Black
    }

    endpoints
  }



}