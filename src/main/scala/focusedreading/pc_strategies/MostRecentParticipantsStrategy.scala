package focusedreading.pc_strategies

import focusedreading.Participant
import focusedreading.models.SearchModel

import scala.collection.mutable

trait MostRecentParticipantsStrategy extends ParticipantChoosingStrategy{
  def participantIntroductions:Map[Participant, Int]

  import Color._

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)],
                              model: SearchModel): (Participant, Participant) = {


    // Uncomment for the simpler more effective implementation
    val mostRecentIteration = participantIntroductions.values.max

    val possibleEndpoints = participantIntroductions map { case(p, i) => if(i == mostRecentIteration) Some(p) else None} collect { case Some(p) => p }


    val sourceComponent = model.getConnectedComponentOf(source).get.toSet
    val destComponent = model.getConnectedComponentOf(destination).get.toSet

    val sourceChoices = possibleEndpoints.filter(p => sourceComponent.contains(p)).toSeq.sortBy(p => model.degree(p)).reverse
    val destChoices = possibleEndpoints.filter(p => destComponent.contains(p)).toSeq.sortBy(p => model.degree(p)).reverse

    val endpoints = pickEndpoints(sourceChoices, destChoices, source, destination, previouslyChosen)

    endpoints
    /////////////////////

  }

  private def pickEndpoints(sourceChoices:Seq[Participant], destChoices:Seq[Participant],
                    source:Participant, destination:Participant,
                   previouslyChosen:Set[(Participant, Participant)]):(Participant, Participant) = {
    val endpoints = {
      val a = if(sourceChoices.nonEmpty) sourceChoices.head else source
      val b = if(destChoices.nonEmpty) destChoices.head else destination

      val candidates = if(a != b)
        (a, b)
      else
        (source, a)

      val elegible = candidates match {
        case (x, y) =>
          val cx = colors.getOrElse(x, White)
          val cy = colors.getOrElse(y, White)

          // To be elegible, one node needs not to be black
        cx != Black || cy != Black
      }

      if(elegible && differentEndpoints(candidates, previouslyChosen))
        candidates
      else if(candidates == (source, destination))
        candidates
      else {
        val newSourceChoices = if(sourceChoices.nonEmpty) sourceChoices.tail else Seq()

        val newDestChoices = if(destChoices.nonEmpty) destChoices.tail else Seq()

        pickEndpoints(newSourceChoices, newDestChoices, source, destination, previouslyChosen)
      }
    }




    endpoints
  }

}