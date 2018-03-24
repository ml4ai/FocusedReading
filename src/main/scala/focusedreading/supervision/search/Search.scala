package focusedreading.supervision.search

import focusedreading.agents.PolicySearchAgent
import focusedreading.supervision.CreateExpertOracle

object Search {

  val groundTruth: Map[(String, String), Option[Seq[(String, String, Seq[String])]]] = CreateExpertOracle.deserialize("shortest_pargs.ser")

  val k1 = groundTruth.keys.head

  val path = groundTruth(k1).get

  val agent = new PolicySearchAgent()
}
