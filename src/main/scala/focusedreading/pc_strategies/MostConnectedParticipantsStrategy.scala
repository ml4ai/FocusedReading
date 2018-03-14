package focusedreading.pc_strategies

import focusedreading.Participant
import focusedreading.models.SearchModel
import focusedreading.pc_strategies.ParticipantChoosingStrategy.Color

import scala.collection.mutable

trait MostConnectedParticipantsStrategy extends ParticipantChoosingStrategy{


  override def choseEndPoints(source:Participant,
                     destination:Participant, colors:mutable.Map[Participant, Color],
                     /*previouslyChosen:Set[(Participant, Participant)],*/
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

    do{
      endpoints = pickEndpoints(ssA, ssB)
      // TODO: Fix the line below
    }while(!differentEndpoints(endpoints, Set()/*previouslyChosen*/) && ssA.nonEmpty && ssB.nonEmpty)

    // Fallback if there are no new nodes in the components
    // TODO: Fix the line below
    if(!differentEndpoints(endpoints, Set()/*previouslyChosen*/)){
      ssA.pushAll(sA map (_._2))
      do{
        endpoints = pickEndpoints(ssA, allNodes)
        // TODO: Fix the line below
      }while(!differentEndpoints(endpoints, Set()/*previouslyChosen*/) && ssA.nonEmpty && allNodes.nonEmpty)
    }


    Seq(endpoints._1, endpoints._2)
  }



}