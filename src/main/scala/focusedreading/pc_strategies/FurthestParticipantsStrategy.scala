package focusedreading.pc_strategies

import focusedreading.Participant
import focusedreading.models.SearchModel

import scala.collection.mutable

trait FurthestParticipantsStrategy extends ParticipantChoosingStrategy{

  val distancesCache = new mutable.HashMap[Participant, Int]()

  override def choseEndPoints(source: Participant, destination: Participant,
                              previouslyChosen: Set[(Participant, Participant)],
                              model: SearchModel): (Participant, Participant) = {

    val sourceComponent = sortByDistance(model.getConnectedComponentOf(source).get.toSeq, source, model)
    val destComponent = sortByDistance(model.getConnectedComponentOf(destination).get.toSeq, destination, model)


    val ssA = new mutable.Stack[Participant]()
    ssA.pushAll(sourceComponent)

    val ssB = new mutable.Stack[Participant]()
    ssB.pushAll(destComponent)

    var endpoints:(Participant, Participant) = (null, null)

    do{
      endpoints = pickEndpoints(ssA, ssB)
    }while(!differentEndpoints(endpoints, previouslyChosen) && ssA.nonEmpty && ssB.nonEmpty)

    endpoints
  }

  private def sortByDistance(s:Seq[Participant], reference:Participant, model:SearchModel):Seq[Participant] = {
    //val maxDist = model.numNodes -1 // This is an upper bound of the distance

    val distances = s map {
      node =>
        if(distancesCache.contains(node))
          Some(distancesCache(node))
        else {
          model.shortestPath(reference, node) match {
            case Some(path) =>
              val dist = path.size
              distancesCache += node -> dist
              Some(dist)
            case None =>
              None
          }
        }
    }

    //val degrees = s map (node => model.degree(node))

    //val scores = distances zip degrees map { case (d, dd) => d +dd }

    val sorted = s.zip(distances).collect{
      case (p, Some(d)) => (p, d)
    }.sortBy(_._2).reverse

    sorted map (_._1)
  }
}
