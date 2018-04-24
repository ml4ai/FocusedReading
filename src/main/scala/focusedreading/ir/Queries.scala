package focusedreading.ir

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.QueryParserBase
import focusedreading.Participant
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

case class Query(strategy:QueryStrategy.Strategy, count:Int, A:Participant, B:Option[Participant])


/**
  * Created by enrique on 18/02/17.
  */
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
      val resolved = LuceneQueries.dict.lift(term) match {
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
  logger.info("Loading KBs...")
  var lines = ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("uniprot-proteins.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("GO-subcellular-locations.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PubChem.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PFAM-families.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("bio_process.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("hgnc.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("hmdb.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("BEfamilies.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("BEcomplexes.tsv.gz")).getLines.toSeq
  lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("NER-Grounding-Override.tsv.gz")).getLines.filterNot(_.startsWith("#")).toSeq


  val dict = lines.map{ l => val t = l.split("\t"); (t(1), t(0)) }.groupBy(t=> t._1).mapValues(l => l.map(_._2).distinct)

  //data/nlp/corpora/pmc_openaccess/pmc_aug2016_index"
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

/**
  * Queries Lucene and caches the results on a Redis server to improve performance
  * @param indexDir
  */
class RedisLuceneQueries(indexDir:String, server:String = "localhost", port:Int = 6379) extends LuceneQueries(indexDir) {

  //logger.info(s"Connecting to Redis @ $server:$port")
  val redisClient = new RedisClient(server, port)

  override def fetchHits(hits: Set[(Int, Float)]): List[(String, Float)] = {

    redisClient.reconnect

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

    redisClient.quit

    existing.toList ++ newElements

  }

  override def binaryConjunctionQuery(a: Participant, b: Participant, totalHits: Option[Int]): Iterable[(String, Float)] = {

    redisClient.reconnect

    val queryKey = s"conjunction:${a.id}:${b.id}:$totalHits"
    val cachedResultSize = redisClient.llen(s"$queryKey:pmcid").get


    if(cachedResultSize > 1) {
      val pmcidOptions = redisClient.lrange(s"$queryKey:pmcid", 0, -1).get
      val irScoreOptions = redisClient.lrange(s"$queryKey:irscore", 0, -1).get

      redisClient.quit

      pmcidOptions.collect { case Some(pmcid) => pmcid } zip irScoreOptions.collect { case Some(irScore) => irScore.toFloat }

    }
    else{
      // Query lucene
      val result= super.binaryConjunctionQuery(a, b, totalHits)
      // Store the results on Redis
      result foreach {
        case (pmcid, irScore) => redisClient.rpush(s"$queryKey:pmcid", pmcid)
          redisClient.rpush(s"$queryKey:irscore", irScore)
      }

      redisClient.quit
      // Return the query result
      result
    }

  }

  override def binaryDisjunctionQuery(a: Participant, b: Participant, totalHits: Int): Iterable[(String, Float)] = {
    redisClient.reconnect
    val queryKey = s"disjunction:${a.id}:${b.id}:$totalHits"
    val cachedResultSize = redisClient.llen(s"$queryKey:pmcid").get


    if(cachedResultSize > 1) {
      val pmcidOptions = redisClient.lrange(s"$queryKey:pmcid", 0, -1).get
      val irScoreOptions = redisClient.lrange(s"$queryKey:irscore", 0, -1).get

      redisClient.quit

      pmcidOptions.collect { case Some(pmcid) => pmcid } zip irScoreOptions.collect { case Some(irScore) => irScore.toFloat }

    }
    else{
        // Query lucene
      val result= super.binaryDisjunctionQuery(a, b, totalHits)
      // Store the results on Redis
      result foreach {
        case (pmcid, irScore) => redisClient.rpush(s"$queryKey:pmcid", pmcid)
          redisClient.rpush(s"$queryKey:irscore", irScore)
      }

      redisClient.quit
      // Return the query result
      result
    }
  }

  override def binarySpatialQuery(a: Participant, b: Participant, k: Int, totalHits: Int): Iterable[(String, Float)] = {
    redisClient.reconnect
    val queryKey = s"spatial:${a.id}:${b.id}:$k:$totalHits"
    val cachedResultSize = redisClient.llen(s"$queryKey:pmcid").get


    if(cachedResultSize > 1) {
      val pmcidOptions = redisClient.lrange(s"$queryKey:pmcid", 0, -1).get
      val irScoreOptions = redisClient.lrange(s"$queryKey:irscore", 0, -1).get

      pmcidOptions.collect { case Some(pmcid) => pmcid } zip irScoreOptions.collect { case Some(irScore) => irScore.toFloat }

    }
    else{
      // Query lucene
      val result= super.binarySpatialQuery(a, b, k, totalHits)
      // Store the results on Redis
      result foreach {
        case (pmcid, irScore) => redisClient.rpush(s"$queryKey:pmcid", pmcid)
          redisClient.rpush(s"$queryKey:irscore", irScore)
      }

      redisClient.quit
      // Return the query result
      result
    }
  }

  override def singletonQuery(p: Participant, totalHits: Int): Iterable[(String, Float)] = {
    redisClient.reconnect
    val queryKey = s"singleton:${p.id}:$totalHits"
    val cachedResultSize = redisClient.llen(s"$queryKey:pmcid").get


    if(cachedResultSize > 1) {
      val pmcidOptions = redisClient.lrange(s"$queryKey:pmcid", 0, -1).get
      val irScoreOptions = redisClient.lrange(s"$queryKey:irscore", 0, -1).get

      redisClient.quit
      pmcidOptions.collect { case Some(pmcid) => pmcid } zip irScoreOptions.collect { case Some(irScore) => irScore.toFloat }

    }
    else{
      // Query lucene
      val result= super.singletonQuery(p, totalHits)
      // Store the results on Redis
      result foreach {
        case (pmcid, irScore) => redisClient.rpush(s"$queryKey:pmcid", pmcid)
          redisClient.rpush(s"$queryKey:irscore", irScore)
      }
      redisClient.quit
      // Return the query result
      result
    }
  }
}