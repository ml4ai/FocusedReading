package focusedreading.pc_strategies.new_age

import focusedreading.Participant
import focusedreading.models.SearchModel
import focusedreading.pc_strategies.ParticipantChoosingStrategy
import focusedreading.pc_strategies.ParticipantChoosingStrategy.Color

import scala.collection.mutable

trait ExploreStrategy extends ParticipantChoosingStrategy{
  override def choseEndPoints(source: Participant, destination: Participant, colors:mutable.Map[Participant, Color], model: SearchModel) = {

    val rankedNodes = model.nodes.toSeq filter {
      node =>
        val color = colors.getOrElse(node, Color.White)
        color != Color.Black
    } sortBy {
      node =>
        -model.degree(node)
    }

    assert(rankedNodes.size > 0, "There should be one choice for exploration")

    val choice = rankedNodes.head

    colors(choice) = Color.Black

    Seq(choice)
  }
}
