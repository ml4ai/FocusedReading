package focusedreading.models
import focusedreading.{Connection, Participant}

import scala.collection.mutable

class EfficientSearchModel extends SearchModel {

  private[EfficientSearchModel] var graph = Map[Participant, List[Connection]]()
  private[EfficientSearchModel] var inDegrees = Map[Participant, Int]()
  private[EfficientSearchModel] var outDegrees = Map[Participant, Int]()
  private[EfficientSearchModel] var _numNodes = 0
  private[EfficientSearchModel] var _numEdges = 0

  def this(source:Participant, destination:Participant) {
    this()
    addNode(source)
    addNode(destination)
  }

  def copy():SearchModel = {
    val clone = new EfficientSearchModel()
    clone.graph = this.graph
    clone.inDegrees = this.inDegrees
    clone.outDegrees = this.outDegrees
    clone._numNodes = this._numNodes
    clone._numEdges = this._numEdges

    clone
  }

  override def addNode(p: Participant): Unit = {
    if(!graph.contains(p)){
      this._numNodes += 1
      graph += p -> Nil
      inDegrees += p -> 0
      outDegrees += p -> 0
    }
  }

  override def addEdge(e: Connection): Unit = addEdges(Seq(e))

  override def addEdges(es: Iterable[Connection]): Unit = {
    // Efficient implementation of adding multiple edges at once
    // Iterate only once per incidence list

    // Figure out which incidence lists to update
    val grouped = es.groupBy(_.controller).mapValues(_.toSet.toList) // TODO: Maybe optimize by taking size before making it a list

    // Now, for each group, by source node, repeat
    grouped foreach {
      case (s, connections) => {

        // Fetch the incidence list
        if(!graph.contains(s)){
          _numEdges += connections.size
          _numNodes += 1
          inDegrees += s -> 0
          outDegrees += s -> connections.length

          for(c <- connections.map(_.controlled)){
            if(!graph.contains(c)){
              graph += c -> Nil
              _numNodes += 1
              inDegrees += c -> 1
              outDegrees += c -> 0
            }
            else{
              inDegrees += c -> (inDegrees(c) + 1)
            }
          }
          graph += s -> connections
        }
        else{
          // Then the graph already contains s
          var incidences = graph(s)
          val toAdd = incidences match {
            case Nil => connections
            case l => {
              val existing = mutable.HashMap[Connection, Boolean]() ++ connections.map(_ -> false).toMap
              for(e <- l){
                if(existing.contains(e)){
                  existing(e) = true
                }
              }
              connections filterNot existing
            }
          }

          outDegrees += s -> (outDegrees(s) + toAdd.length)
          _numEdges += toAdd.size

          for(c <- toAdd){
            incidences = c::incidences
            if(!graph.contains(c.controlled)){
              graph += c.controlled -> Nil
              _numNodes += 1
              inDegrees += c.controlled -> 1
              outDegrees += c.controlled -> 0
            }
            else{
              inDegrees += c.controlled -> (inDegrees(c.controlled) + 1)
            }
          }

          graph += s -> incidences
        }
      }
    }
  }

  override def nodes: Iterable[Participant] = graph.keys

  override def edges: Iterable[Connection] = graph.values.flatten

  override def numNodes: Int = this._numNodes

  override def numEdges: Int = this._numEdges

  override def getConnectedComponentOf(node: Participant): Option[Iterable[Participant]] = graph.contains(node) match {
    case false => None
    case true => Some(breathFirstWalk(node))
  }

  override def shortestPath(source: Participant, destination: Participant): Option[Seq[Connection]] = dijkstrasWalk(source, destination)

  override def degree(node: Participant): Int = inDegree(node) + outDegree(node)

  override def rankedNodes: Map[Participant, Int] = nodes.map(n => n -> degree(n)).toMap

  // Not used yet
  override def allPaths(source: Participant, destination: Participant): Iterable[Seq[Connection]] = ???

  // Not used yet
  override def connectedComponents(): Iterable[Set[Participant]] = ???


  private def inDegree(p:Participant):Int = inDegrees.getOrElse(p, 0)
  private def outDegree(p:Participant):Int = outDegrees.getOrElse(p, 0)
  private def breathFirstWalk(start:Participant):Iterable[Participant]  = {
    val queue = new mutable.Queue[Participant]()
    val explored = new mutable.HashSet[Participant]()
    queue.enqueue(start)

    var result = List.empty[Participant]

    while(queue.nonEmpty){
      val current = queue.dequeue()
      result = current::result
      if(!explored.contains(current)){
        explored += current
        for(d <- graph(current)){
          queue.enqueue(d.controlled)
        }
      }
    }

    result
  }

  private def dijkstrasWalk(start:Participant, end:Participant):Option[Seq[Connection]] = {
    if(graph.contains(start) && graph.contains(end)) {
      val maxDist = numNodes + 1

      val previousElement = new mutable.HashMap[Participant, Option[Connection]]() + (start -> None)
      val explored = new mutable.HashSet[Participant]()

      val distances = new mutable.HashMap[Participant, Int] {
        override def default(key: Participant): Int = maxDist
      }

      implicit object ParticipantOrdering extends Ordering[Participant] {
        override def compare(x: Participant, y: Participant): Int = distances(x) compare distances(y)
      }

      val queue = new mutable.PriorityQueue[Participant]()

      queue.enqueue(start)
      distances(start) = 0

      var shortCircuit = false

      while (queue.nonEmpty && !shortCircuit) {

        val current = queue.dequeue()

        if (current == end) {
          shortCircuit = true
        }
        else {
          if (!explored.contains(current)) {
            explored += current
            val neighbors = graph(current)
            for (n <- neighbors) {
              val ct = n.controlled
              val alt = 1 + distances(current)
              val dist = distances(ct)

              if (alt < dist) {
                distances(ct) = alt
                previousElement(ct) = Some(n)
                queue.enqueue(ct)
              }
            }
          }
        }

      }


      distances(end) match {
        case l if l == maxDist => None
        case _ => {
          var path = List[Connection]()
          var current = end

          while (previousElement(current).isDefined) {
            val next = previousElement(current).get
            path = next :: path
            current = next.controller
          }

          Some(path)
        }


      }
    }
    else
      None
  }



}
