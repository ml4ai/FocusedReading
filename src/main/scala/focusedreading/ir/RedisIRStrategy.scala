package focusedreading.ir

import focusedreading.ir.queries.{Query, RedisLuceneQueries}
import focusedreading.ir.queries.QueryStrategy._

trait RedisIRStrategy extends IRStrategy{

  val indexDir:String

  val redisLuceneQuerier = new RedisLuceneQueries(indexDir)

  override def informationRetrieval(query: Query): Iterable[(String, Float)] = {

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