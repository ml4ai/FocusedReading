package focusedreading.ir

import focusedreading.ir.queries.Query
import focusedreading.ir.queries.QueryStrategy._
import focusedreading.sqlite.SQLiteQueries

trait SQLIRStrategy extends IRStrategy{

  val dbPath:String

  val daIR = new SQLiteQueries(dbPath)

  override def informationRetrieval(query: Query): Iterable[(String, Float)] = {
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
