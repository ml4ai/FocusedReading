package focusedreading.supervision.search.executable

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.sql.DriverManager

import focusedreading.Configuration
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph

import scala.collection.mutable
import scala.io.Source



object CreateExpertOracle extends App{

  val dbPath = Configuration.SQLite.dbPath

  println(s"Finding paths for training data exper from: $dbPath")

  // Load the JDBC driver
  Class.forName("org.sqlite.JDBC")

  private val getConnection = DriverManager.getConnection(s"jdbc:sqlite:$dbPath");

  // Read all the edges and the papers they appear at
  val CMD = s"SELECT controller, controlled, group_concat(pmcid) AS papers FROM Interactions as i JOIN Paper_Interaction as pi ON i.id = pi.interaction GROUP BY controller, controlled;"

  val conn = getConnection
  val cmd = conn.prepareStatement(CMD)

  var resultSet = cmd.executeQuery

  val interactions = new mutable.ArrayBuffer[(String, String, Seq[String])] // Interaction ID, Controller, Controlled, Sign, Frequency

  while(resultSet.next){
    val controller = resultSet.getString("controller")
    val controlled = resultSet.getString("controlled")
    val pmcids = resultSet.getString("papers").split(",")

    interactions += Tuple3(controller, controlled, pmcids)
  }

  resultSet.close()
  cmd.close()

  println(s"Total number of interactions: ${interactions.size}")

  // Build a graph out of those results
  val G = Graph[String, LDiEdge]()

  G ++= interactions map {
    case (controller, controlled, papers) => (controller ~+> controlled)(papers)
  }

  def getShortestPath(source:String, destination:String):Option[Seq[(String, String, Seq[String])]] = {
    println(s"Finding shortest for $source -> $destination")
    val pa = G.get(source).asInstanceOf[G.NodeT]
    val pb = G.get(destination).asInstanceOf[G.NodeT]

    val shortest = pa shortestPathTo pb

    val truth = shortest match {
      case Some(path) =>
        val ret = path.edges.map{
          e:G.EdgeT => (e.source.value, e.target.value, e.label.asInstanceOf[Seq[String]])
        }.toSeq

        Some(ret)
      case None =>
        println(s"Couldn't find for $source -> $destination")
        None
    }

    truth
  }

  def getPath(source: String, destination:String, restriction:Option[G.EdgeT] = None):Option[Seq[(String, String, Seq[String])]] = {
    println(s"Finding a path for $source -> $destination")
    val pa = G.get(source).asInstanceOf[G.NodeT]
    val pb = G.get(destination).asInstanceOf[G.NodeT]

    val shortest = restriction match {
      case None =>
        pa pathTo pb
      case Some(e) =>
        pa.withSubgraph(edges = _ != e) pathTo pb
    }


    val truth = shortest match {
      case Some(path) =>
        val ret = path.edges.map{
          e:G.EdgeT => (e.source.value, e.target.value, e.label.asInstanceOf[Seq[String]])
        }.toSeq

        Some(ret)
      case None =>
        println(s"Couldn't find for $source -> $destination")
        None
    }

    truth
  }

  def findGroundTruth(pathways:List[Array[String]]):Map[(String, String), Option[Seq[(String, String, Seq[String])]]]  = {

    println("Finding the ground-truth segments")

    val startingTime = System.nanoTime()

    val pathwayInteractions = pathways.map(p => p -> p.sliding(2).map( i=> (i(0), i(1)))).toMap

    val shortestPaths = pathwayInteractions.values.flatten.toSet[(String, String)].par.map{
      case (s, d) =>
        (s, d) -> getShortestPath(s, d)
    }.seq.toMap


    val filePath = Configuration.ExpertOracle.goldenDataPath
    serialize(shortestPaths, filePath)

    val endingTime = System.nanoTime()
    val totalTime = endingTime - startingTime
    println(s"Took ${totalTime/10E8} seconds")

    shortestPaths
  }

  def findNegativeExamples(pathways:List[Array[String]], goldenPaths:Map[(String, String), Option[Seq[(String, String, Seq[String])]]] ):Map[(String, String), Option[Seq[(String, String, Seq[String])]]]  = {
    println("Finding the negative examples")

    val startingTime = System.nanoTime()

    val negativeExamples = pathways.map{ p=> (p.head, p.last)}.toSet
    val shortestPaths = negativeExamples.par.map {
      case (s, d) => {
        val restriction = if(goldenPaths.contains((s, d))){
          val g = goldenPaths((s, d)) match {
            case Some(path) => {
              val l = path.last
              val e = G.find((l._1 ~+> l._2)(l._3))
              e
            }
            case None =>
              None
          }
          g
        }
        else{
          None
        }

        (s, d) -> getPath(s, d, restriction)
      }
    }.seq.toMap


    val filePath = Configuration.ExpertOracle.negativeDataPath
    serialize(shortestPaths, filePath)

    val endingTime = System.nanoTime()
    val totalTime = endingTime - startingTime
    println(s"Took ${totalTime/10E8} seconds")

    shortestPaths
  }

  def serialize(collection:Any, path:String):Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    oos.writeObject(collection)
    oos.close()
  }


  def buildDatasets():Unit = {
    val filePath = Configuration.ExpertOracle.inputPath

    val pathways = io.Source.fromFile(filePath).getLines.toList.map(_.split("\t"))


    val goldenDataPath = Configuration.ExpertOracle.goldenDataPath
    val gt = findGroundTruth(pathways)
    //val gt = deserialize(goldenDataPath)
    findNegativeExamples(pathways, gt)
  }


  def deserialize(path:String):Map[(String, String), Option[Seq[(String, String, Seq[String])]]] = {
    val ois = new ObjectInputStream(new FileInputStream(path))
    val ret = ois.readObject().asInstanceOf[Map[(String, String), Option[Seq[(String, String, Seq[String])]]]]
    ois.close()

    ret
  }

  def reassemblePaths(sequencesPath:String, fragments:Map[(String, String), Option[Seq[(String, String, Seq[String])]]]) = {

    val trainingPaths = Source.fromFile(sequencesPath).getLines().toList.map(_.split("\t")).map(s => s.sliding(2).toList)

    val trainingData = trainingPaths.map{
      s =>
        val key = (s.head(0), s.last(1))
        val sequence = s.flatMap{
          p =>
            fragments((p(0), p(1))).get
        }

        key -> sequence
    }.toMap

    trainingData
  }

  def reassemblePaths(sequencesPath:String, fragmentsPath:String):Map[(String, String), List[(String, String, Seq[String])]] = {
    val fragments = deserialize(fragmentsPath)
    reassemblePaths(sequencesPath, fragments)
  }

  buildDatasets()
}
