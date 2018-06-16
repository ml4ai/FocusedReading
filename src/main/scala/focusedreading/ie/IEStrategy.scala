package focusedreading.ie

import com.typesafe.scalalogging.LazyLogging
import focusedreading.entities.Connection

/**
  * Created by enrique on 20/02/17.
  */
trait IEStrategy extends LazyLogging {
  def informationExtraction(pmcids: Iterable[String]):Iterable[Connection]
  def getEvidence(connection:Connection):Iterable[String]
}
