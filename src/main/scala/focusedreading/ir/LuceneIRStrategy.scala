package focusedreading.ir

import focusedreading.ir.queries.{LuceneQueries, Query}
import focusedreading.ir.queries.QueryStrategy._

trait LuceneIRStrategy extends IRStrategy{
  val maxHits = 100
  val indexDir:String

  val luceneQuerier = new LuceneQueries(indexDir)

  override def informationRetrieval(query: Query): Iterable[(String, Float)] = {
    val pmcids:Iterable[(String, Float)] = query.strategy match {
      case Singleton => luceneQuerier.singletonQuery(query.A, maxHits)
      case Disjunction => luceneQuerier.binaryDisjunctionQuery2(query.A, query.B.get, maxHits)
      case Conjunction => luceneQuerier.binaryConjunctionQuery(query.A, query.B.get, Some(maxHits))
      case Spatial => luceneQuerier.binarySpatialQuery(query.A, query.B.get, 20, maxHits) // TODO parameterize that 20
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
