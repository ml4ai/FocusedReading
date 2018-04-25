package focusedreading.supervision.search.analysis

import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import org.clulab.utils.Serializer
/**
  * Inspects the solutions that come out of a search process, such as UCS, A*, etc
  * Takes the serialized files as command line parameters
  */
object SearchSolutionsAnalysis extends App {

  type SolutionsMap = Map[(String, String), Option[Seq[(FocusedReadingState, FocusedReadingAction)]]]

  def printStatistics(collection: Iterable[Int]): Unit = {
    println(s"Min: ${collection.min}")
    println(s"Max: ${collection.max}")
    val avg = collection.sum / collection.size.toDouble
    println(s"Avg: $avg")
    val std = Math.sqrt(collection.map(x => Math.pow(x-avg, 2)).sum / (collection.size - 1))
    println(s"Std: $std")
  }

  def printFrequencies(collection: Iterable[Any]): Unit = {
    println("Frequencies:")
    val groups = collection.groupBy(identity).mapValues(_.size).toSeq.sortBy{case (k, v) => v}.reverse
    groups foreach {
      case (k, v) =>
        println(s"$k: $v")
    }
  }

  if(args.isEmpty){
    println("Must pass the solutions file as a parameter")
  }
  else{
    val solutionsPath = args(0)

    val dataSet = Serializer.load[SolutionsMap](solutionsPath)

    val solutions = dataSet.values.collect{case Some(s) => s}

    // Size information
    println(s"Total num of problems: ${dataSet.size}")
    println(s"Problems with solution: ${solutions.size}")
    println(s"Problems without solution: ${dataSet.size - solutions.size}")
    println()

    // Solution lengths statistics
    val solutionLengths = solutions.map(_.length)

    println("Solution length statistics")
    printStatistics(solutionLengths)
    println()
    printFrequencies(solutionLengths)
    println()

    val sequences = solutions.map{_.map{case (s, a) => a}}

    // Action frequency statistics
    val actions = sequences.flatten
    println("Action frequency statistics")
    printFrequencies(actions)
    println()

//    // Action sequence statistics
//    println("Solution sequences statistics")
//    println(s"Different sequences: ${sequences.toSet.size}")
//    printFrequencies(sequences)
  }

}
