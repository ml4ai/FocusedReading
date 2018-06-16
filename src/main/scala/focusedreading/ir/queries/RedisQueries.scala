package focusedreading.ir.queries

import com.redis.RedisClient
import focusedreading.entities.Participant

/**
  * Queries Lucene and caches the results on a Redis server to improve performance
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
