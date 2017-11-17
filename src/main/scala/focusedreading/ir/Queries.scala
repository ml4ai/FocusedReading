package org.clulab.reach.focusedreading.ir

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.QueryParserBase
import org.clulab.reach.focusedreading.Participant
import org.clulab.reach.grounding.ReachKBUtils
//import org.clulab.reach.indexer.NxmlSearcher
import org.clulab.utils.Serializer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import com.redis._

object QueryStrategy extends Enumeration{
  type Strategy = Value
  val Singleton, Disjunction, Conjunction, Spatial, Cascade = Value
}

case class Query(val strategy:QueryStrategy.Strategy, val A:Participant, val B:Option[Participant])


/**
  * Created by enrique on 18/02/17.
  */
class LuceneQueries(indexDir:String) extends LazyLogging{

  //data/nlp/corpora/pmc_openaccess/pmc_aug2016_index"
  val nxmlSearcher:NxmlSearcher = new NxmlSearcher(indexDir)
  val nxmlDir = "/work/enoriega/fillblanks/nxml"

  /***
    * Gets the synonyms from the KB files
    * @param term Grounding ID without namespace to look for
    * @return String with the disjunction of synonyms ready to be queried by lucene
    */
  def resolveParticipant(term:String) = {

    LuceneQueries.dict.lift(term) match {
      case Some(l) => "(" + l.map( x => "\"" + x + "\"").mkString(" OR ") + ")"
      case None =>
        logger.debug(s"Warning: missing term in the KB: $term")
        ""
    }
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
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHits(hits)
  }

  def binaryConjunctionQuery(a:Participant, b:Participant, totalHits:Int):Iterable[(String, Float)] = {
    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    var luceneQuery = QueryParserBase.escape("(" + aSynonyms + " AND " + bSynonyms + ")")
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHits(hits)
  }

  def binaryDisonjunctionQuery(a:Participant, b:Participant, totalHits:Int):Iterable[(String, Float)] = {
    // Build a query for lucene
    val aSynonyms = resolveParticipant(a.id)
    val bSynonyms = resolveParticipant(b.id)

    if(aSynonyms.isEmpty || bSynonyms.isEmpty){
      return Set()
    }

    var luceneQuery = QueryParserBase.escape("(" + aSynonyms + " OR " + bSynonyms + ")")
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHits(hits)
  }

  def singletonQuery(p:Participant, totalHits:Int):Iterable[(String, Float)] = {
    val synonyms = resolveParticipant(p.id)

    if(synonyms.isEmpty)
      return Set()

    var luceneQuery = QueryParserBase.escape("(" + synonyms + ")")
    var hits = nxmlSearcher.searchByField(luceneQuery, "text", new StandardAnalyzer(), totalHits) // Search Lucene for the participants

    fetchHits(hits)
  }

}

object LuceneQueries extends LazyLogging {
  logger.info("Loading KBs...")
  var lines = ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("uniprot-proteins.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("GO-subcellular-locations.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PubChem.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PFAM-families.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("bio_process.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("hgnc.tsv.gz")).getLines.toSeq

  val dict = lines.map{ l => val t = l.split("\t"); (t(1), t(0)) }.groupBy(t=> t._1).mapValues(l => l.map(_._2).distinct)

}

/**
  * Queries Lucene and caches the results on a Redis server to improve performance
  * @param indexDir
  */
class RedisLuceneQueries(indexDir:String, server:String = "localhost", port:Int = 6379) extends LuceneQueries(indexDir) {

  logger.info(s"Connecting to Redis @ $server:$port")
  val redisClient = new RedisClient(server, port)

  override def fetchHits(hits: Set[(Int, Float)]): List[(String, Float)] = {

    val orderedHits = hits.toSeq
    val cachedElements = orderedHits map (h => s"id:${h._1}") map redisClient.get[String]

    val zipped = orderedHits zip cachedElements

    val existing = zipped collect {
      case ((id:Int, ir:Float), Some(pmcid)) => (pmcid, ir)
    }

    val pending = zipped collect {
      case ((id, ir), None) => (id, ir)
    }


    // Fetch the pending values from Lucene and cache them in Redis
    val docs = nxmlSearcher.docs(pending)
    val newElements = docs.map(d => (d._1.get("id"), d._2))

    pending.map(_._1) zip newElements.map(_._1) foreach {
      case (id, pmcid) =>
        redisClient.set(s"id:$id", pmcid)
    }


    existing.toList ++ newElements

  }

//  override def binaryConjunctionQuery(a: Participant, b: Participant, totalHits: Int): Iterable[(String, Float)] = {
//
//    super.binaryConjunctionQuery(a, b, totalHits)
//  }
}