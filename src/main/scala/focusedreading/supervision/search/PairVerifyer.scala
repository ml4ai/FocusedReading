package focusedreading.supervision.search

import com.typesafe.config.ConfigFactory
import focusedreading.Participant
import focusedreading.ir.LuceneQueries

object PairVerifyer extends App {

  val config = ConfigFactory.load()

  val pairs = io.Source.fromFile("pairs_training").getLines().map{
    s =>
      val tokens = s.split(",")
      (tokens(0).stripPrefix(":"), tokens(1).stripPrefix(":"))
  }.toList

  val participants = pairs.flatMap{case (a, b) => Seq(a, b)}.toSet

  val indexPath = config.getConfig("lucene").getString("annotationsIndex")
  val querier = new LuceneQueries(indexPath)

  val hasSynonyms = participants.map(p => p -> !querier.resolveParticipant(p).isEmpty).toMap

  for((k, v) <- hasSynonyms if !v){
    println(s"$k doesn't have synonyms")
  }

  println()

  // Now explain the pairs that have both elements with no synonyms
  val groups = pairs.groupBy{
    case (a, b) =>
      if(hasSynonyms(a) && hasSynonyms(b))
        "bothSyn"
      else if(!hasSynonyms(a) && !hasSynonyms(b))
        "noneSyn"
      else
        "someSyn"
  }

  val someSyn = groups("someSyn")

  for((a, b) <- someSyn){
    val ret = querier.binaryConjunctionQuery(Participant("", a), Participant("", b), None)
    if(ret.isEmpty){
      val i = 0
    }
  }

  val x = 0
  // First, which participants don't have synonyms

  // Second, which pairs, with non empty synonyms, have no conjunction


}
