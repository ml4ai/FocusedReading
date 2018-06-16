package focusedreading.ir.queries

object QueryStrategy extends Enumeration{
  type Strategy = Value
  val Singleton, Disjunction, Conjunction, Spatial, Cascade = Value
}
