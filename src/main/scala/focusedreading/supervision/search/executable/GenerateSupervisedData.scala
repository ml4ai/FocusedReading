package focusedreading.supervision.search.executable

import java.io.{BufferedOutputStream, FileOutputStream, PrintWriter}

import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import focusedreading.supervision.search.analysis.SearchSolutionsAnalysis.SolutionsMap
import org.clulab.utils.Serializer

/**
  * Generates a text file to use with classification programs
  */
object GenerateSupervisedData extends App {

  type SolutionsMap = Map[(String, String), Option[Seq[(FocusedReadingState, FocusedReadingAction, Double)]]]

  if(args.isEmpty){
    println("Must pass the solutions file as a parameter")
  }
  else {
    val solutionsPath = args(0)

    val dataSet = Serializer.load[SolutionsMap](solutionsPath)

    val rawDataPoints = dataSet.values.collect{case Some(s) => s}.flatMap{
      case items =>
        items.map{
          case (state, action, _) => (state.toFeatures(), action)
        }
    }

    val featureNames = rawDataPoints.head._1.keys.toSeq.sorted

    val header = "label\t" + featureNames.mkString("\t")

    val lines = rawDataPoints map {
      case (features, label) => {
        val values = featureNames map features mkString "\t"
        s"$label\t$values"
      }
    }

    val osw = new PrintWriter(new FileOutputStream("features.txt"))

    osw.println(header)
    lines foreach osw.println

    osw.close

  }

}
