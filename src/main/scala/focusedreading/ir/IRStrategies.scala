package focusedreading.ir

import QueryStrategy._
import focusedreading.sqlite.SQLiteQueries

/**
  * Created by enrique on 20/02/17.
  */
trait IRStrategy {
  def informationRetrival(query: Query):Iterable[(String, Float)]
}


trait LuceneIRStrategy extends IRStrategy{
  val maxHits = 100
  val indexDir:String

  val luceneQuerier = new LuceneQueries(indexDir)

  override def informationRetrival(query: Query) = {
    val pmcids:Iterable[(String, Float)] = query.strategy match {
      case Singleton => luceneQuerier.singletonQuery(query.A, maxHits)
      case Disjunction => luceneQuerier.binaryDisjunctionQuery2(query.A, query.B.get, maxHits)
      case Conjunction => luceneQuerier.binaryConjunctionQuery(query.A, query.B.get, Some(maxHits))
      case Spatial => luceneQuerier.binarySpatialQuery(query.A, query.B.get, 20, maxHits)
      case Cascade => {
        var results = luceneQuerier.binarySpatialQuery(query.A, query.B.get, 20, maxHits)
        if(results.isEmpty){
          results = luceneQuerier.binaryConjunctionQuery(query.A, query.B.get, Some(maxHits))
          if(results.isEmpty)
            results = luceneQuerier.binaryDisjunctionQuery2(query.A, query.B.get, maxHits)
        }
        results
      }

    }


    pmcids
  }
}

trait SQLIRStrategy extends IRStrategy{

  def dbPath:String

  val daIR = new SQLiteQueries(dbPath)

  override def informationRetrival(query: Query) = {
    val pmcids: Iterable[String] = query.strategy match {
      case Singleton => daIR.singletonQuery(query.A)
      case Disjunction => daIR.binaryDisjunctionQuery(query.A, query.B.get)
      case Conjunction => daIR.binaryConjunctionQuery(query.A, query.B.get)
      case Spatial => daIR.binarySpatialQuery(query.A, query.B.get)
      case Cascade => {
        var results = daIR.binarySpatialQuery(query.A, query.B.get)
        if (results.isEmpty) {
          results = daIR.binaryConjunctionQuery(query.A, query.B.get)
          if (results.isEmpty)
            results = daIR.binaryDisjunctionQuery(query.A, query.B.get)
        }
        results
      }
    }

    pmcids map (p => (p, 0.0f))
  }
}


trait RedisIRStrategy extends IRStrategy{

  def indexDir:String

  val redisLuceneQuerier = new RedisLuceneQueries(indexDir)

  override def informationRetrival(query: Query) = {

    val pmcids:Iterable[(String, Float)] = query.strategy match {
      case Singleton => redisLuceneQuerier.singletonQuery(query.A, query.count)
      case Disjunction => redisLuceneQuerier.binaryDisjunctionQuery(query.A, query.B.get, query.count)
      case Conjunction => redisLuceneQuerier.binaryConjunctionQuery(query.A, query.B.get, Some(query.count))
      case Spatial => redisLuceneQuerier.binarySpatialQuery(query.A, query.B.get, 20, query.count)
      case Cascade => {
        var results = redisLuceneQuerier.binarySpatialQuery(query.A, query.B.get, 20, query.count)
        if(results.isEmpty){
          results = redisLuceneQuerier.binaryConjunctionQuery(query.A, query.B.get, Some(query.count))
          if(results.isEmpty)
            results = redisLuceneQuerier.binaryDisjunctionQuery(query.A, query.B.get, query.count)
        }
        results
      }

    }


    pmcids
  }
}
