package focusedreading.ir.queries

import com.typesafe.scalalogging.LazyLogging
import focusedreading.KnowledgeBases
import focusedreading.entities.Participant
import focusedreading.ir.NxmlSearcher
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.QueryParserBase
import org.clulab.reach.grounding.ReachKBUtils

import scala.collection.mutable

class LuceneQueries(indexDir:String) extends LazyLogging{

  //data/nlp/corpora/pmc_openaccess/pmc_aug2016_index"
  val nxmlSearcher:NxmlSearcher = LuceneQueries.getSearcher(indexDir)
  val nxmlDir = "/work/enoriega/fillblanks/nxml"

  /***
    * Gets the synonyms from the KB files
    * @param term Grounding ID without namespace to look for
    * @return String with the disjunction of synonyms ready to be queried by lucene
    */
  def resolveParticipant(term:String) = {

    if(!LuceneQueries.participantCache.contains(term)) {
      val resolved = KnowledgeBases.dict.lift(term) match {
        case Some(l) => "(" + l.map(x => "\"" + x + "\"").mkString(" OR ") + ")"
        case None =>
          logger.debug(s"Warning: missing term in the KB: $term")
          ""
      }
      LuceneQueries.participantCache += term -> resolved
      resolved
    }
    else
      LuceneQueries.participantCache(term)
  }

  /***
    * Retrieves documents from lucene.
    * @param hits Set of documents coming from NxmlSearcher
    * @return list with the ids and IR scores of documents
    */
  def fetchHits(hits: Set[(Int, Float)]): List[(String, Float)] = {
    // Hits are tuples with (docId, score), fetch the documents from the ids if they haven't been fetched before

    // Fetch the Document objects
    val docs = nxmlSearcher.docs(hits.toSeq)
    // Get their PMCID and their IR score
    val results = docs.map(d => (d._1.get("id"), d._2))

    results.toList
  }

  /***
    * Expands the frontier with a focus on finding info that may create a path between participants
    * @param a Participant A
    * @param b Participant B
    * @return
    */
  def binarySpatialQuery(a:Participant, b:Participant, k:Int, totalHits:Int):Iterable[(String, Float)] = {


    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    var luceneQuery = QueryParserBase.escape("(" + aSynonyms + " AND " + bSynonyms + ")~"+k)
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), Some(totalHits)) // Search Lucene for the participants

    fetchHits(hits)
  }

  def binaryConjunctionQuery(a:Participant, b:Participant, totalHits:Option[Int]):Iterable[(String, Float)] = {

    val key = (a.id, b.id, "conjunction")

    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    val existingString = LuceneQueries.queryStringCache.lift(key)

    val luceneQuery = existingString match {
      case Some(s) => s
      case None => {
        var query = QueryParserBase.escape("(" + aSynonyms + " AND " + bSynonyms + ")")
        LuceneQueries.queryStringCache += key -> query
        query
      }
    }


    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHits(hits)
  }

  def binaryDisjunctionQuery2(a:Participant, b:Participant, totalHits:Int):Iterable[(String, Float)] = {


    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    var paRes = this.singletonQuery(a, totalHits)
    var pbRes = this.singletonQuery(b, totalHits)

    paRes ++ pbRes
  }

  def binaryDisjunctionQuery(a:Participant, b:Participant, totalHits:Int):Iterable[(String, Float)] = {

    val key = (a.id, b.id, "disjunction")

    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    val existingString = LuceneQueries.queryStringCache.lift(key)

    var luceneQuery =  existingString match {
      case Some(s) => s
      case None => {
        val query = QueryParserBase.escape("(" + aSynonyms + " OR " + bSynonyms + ")")
        LuceneQueries.queryStringCache += key -> query
        query
      }
    }

    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), Some(totalHits)) // Search Lucene for the participants

    fetchHits(hits)
  }

  def singletonQuery(p:Participant, totalHits:Int):Iterable[(String, Float)] = {
    val key = (p.id, p.id, "singleton")
    val synonyms = resolveParticipant(p.id)

    if(synonyms.isEmpty)
      return Set()

    val existingString = LuceneQueries.queryStringCache.lift(key)

    var luceneQuery = existingString match {
      case Some(s) => s
      case None => {
        val query = QueryParserBase.escape("(" + synonyms + ")")
        LuceneQueries.queryStringCache += key -> query
        query
      }
    }
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), Some(totalHits)) // Search Lucene for the participants

    fetchHits(hits)
  }

}

object LuceneQueries extends LazyLogging {

  private val searchers = new mutable.HashMap[String, NxmlSearcher]()

  def getSearcher(indexDir:String):NxmlSearcher = {
    if(!searchers.contains(indexDir)){
      val searcher = new NxmlSearcher(indexDir)
      searchers += indexDir -> searcher
      searcher
    }
    else
      searchers(indexDir)
  }

  val participantCache = new mutable.HashMap[String, String]() with mutable.SynchronizedMap[String, String]
  val queryStringCache = new mutable.HashMap[(String, String, String), String] with mutable.SynchronizedMap[(String, String, String), String]

}
