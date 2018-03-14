package focusedreading.pc_strategies

import focusedreading.Participant
import focusedreading.models.SearchModel
import focusedreading.pc_strategies.ParticipantChoosingStrategy.Color

import scala.collection.mutable

trait MostRecentParticipantsStrategy extends ParticipantChoosingStrategy{
  def participantIntroductions:mutable.HashMap[Participant, Int]

  override def choseEndPoints(source: Participant, destination: Participant,
                              color:mutable.Map[Participant, Color],
                              /*previouslyChosen: Set[(Participant, Participant)],*/
                              model: SearchModel): Seq[Participant] = {


    // Uncomment for the simpler more effective implementation
    val mostRecentIteration = participantIntroductions.values.max

    val possibleEndpoints = participantIntroductions map { case(p, i) => if(i == mostRecentIteration) Some(p) else None} collect { case Some(p) => p }


    val sourceComponent = model.getConnectedComponentOf(source).get.toSet
    val destComponent = model.getConnectedComponentOf(destination).get.toSet

    val sourceChoices = possibleEndpoints.filter(p => sourceComponent.contains(p)).toSeq.sortBy(p => model.degree(p)).reverse
    val destChoices = possibleEndpoints.filter(p => destComponent.contains(p)).toSeq.sortBy(p => model.degree(p)).reverse

    // TODO: Fix the line below
    val endpoints = pickEndpoints(sourceChoices, destChoices, source, destination, Set()/*previouslyChosen*/)

    Seq(endpoints._1, endpoints._2)
    /////////////////////

  }

  private def pickEndpoints(sourceChoices:Seq[Participant], destChoices:Seq[Participant],
                    source:Participant, destination:Participant,
                   previouslyChosen:Set[(Participant, Participant)]):(Participant, Participant) = {
    val endpoints = {
      val a = if(sourceChoices.size > 0) sourceChoices.head else source
      val b = if(destChoices.size > 0) destChoices.head else destination

      val candidates = if(a != b)
        (a, b)
      else
        (source, a)

      if(differentEndpoints(candidates, previouslyChosen))
        candidates
      else if(candidates == (source, destination))
        candidates
      else {
        val newSourceChoices = if(sourceChoices.size > 0) sourceChoices.tail else Seq()

        val newDestChoices = if(destChoices.size > 0) destChoices.tail else Seq()

        pickEndpoints(newSourceChoices, newDestChoices, source, destination, previouslyChosen)
      }
    }




    endpoints
  }

}