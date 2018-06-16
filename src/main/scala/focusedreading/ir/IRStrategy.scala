package focusedreading.ir

import focusedreading.ir.queries.Query

/**
  * Created by enrique on 20/02/17.
  */
trait IRStrategy {
  def informationRetrieval(query: Query):Iterable[(String, Float)]
}
