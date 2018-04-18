package focusedreading.supervision.search.executable

import java.io.{FileInputStream, ObjectInputStream}

import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.supervision.CreateExpertOracle
import focusedreading.supervision.search.FRSearchState.GoldDatum

object Analysis extends App{

  val groundTruth: Map[(String, String), Option[GoldDatum]] = CreateExpertOracle.deserialize("shortest_paths.ser")

  val ios = new ObjectInputStream(new FileInputStream("solutions.ser"))
  val solutions = ios.readObject().asInstanceOf[Map[(String, String), Option[Seq[FocusedReadingAction]]]]
  ios.close()

  //assert(groundTruth.keySet == solutions.keySet, "Solutions don't correspond to data")

  val total = solutions.size
  val withSolution = solutions.collect{ case(k, Some(v)) => (k, v) }

  val numNoSolution = total - withSolution.size

  println(s"$total data points and $numNoSolution with solution")

  println()
  val lengthDistribution = withSolution.values.map(_.length).groupBy(identity).mapValues(_.size).toSeq.sortBy(_._1)
  println("Solution length distribution:")
  lengthDistribution foreach {
    case(k, v) =>
      println(s"Length $k:\t$v")
  }
  println()

  val solutionDistribution = withSolution.values.groupBy(identity).mapValues(_.size).toSeq.sortBy(x => (x._2, -x._1.size)).reverse

  println()
  println("Solution distribution:")
  solutionDistribution foreach {
    case(k, v) =>
      println(s"${k.mkString(" - ")}:\t$v")
  }
  println()

}
