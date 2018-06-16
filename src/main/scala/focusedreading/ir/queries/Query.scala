package focusedreading.ir.queries

import focusedreading.entities.Participant

case class Query(strategy:QueryStrategy.Strategy, count:Int, A:Participant, B:Option[Participant])
