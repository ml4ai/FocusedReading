package focusedreading.supervision.search

import focusedreading.supervision.search.executable.Node

class IterativeLengtheningSearch(initialState:FRSearchState, startingCost:Double,
                                 increment:Double, maxCost:Double){

  def solve():Option[Node] = {
    var solution:Option[Node] = None

    var costBound = startingCost
    do{
      println(s"Doing ILS with cost bound of: $costBound")
      val searcher = new UniformCostSearch(initialState, costBound)
      solution = searcher.solve()
      costBound += increment
    }while(solution.isEmpty && costBound <= maxCost)

    solution
  }

}
