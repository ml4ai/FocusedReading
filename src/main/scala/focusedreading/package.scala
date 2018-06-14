import focusedreading.reinforcement_learning.actions.FocusedReadingAction
import focusedreading.reinforcement_learning.states.FocusedReadingState
import org.sarsamora.actions.Action
import org.sarsamora.states.State

import scala.util.Random

/**
  * General utility functions and values for FocusedReading
  */
package object focusedreading {

  private val _random = new Random(0)

  /**
    * Thread-safe access to the random instance
    */
  private def random:Random = synchronized {
    _random
  }

  /**
    * Singleton object that holds implicit conversion functions
    */
  object implicits {
    /**
      * Implicit cast of sarsamora state to fr state
      * @param s state instance
      * @return casted instance
      */
    implicit def state2frState(s: State): FocusedReadingState = s.asInstanceOf[FocusedReadingState]

    /**
      * Implicit cast of sarsamora action to fr action
      * @param a action instance
      * @return casted instance
      */
    implicit def action2frAction(a: Action): FocusedReadingAction = a.asInstanceOf[FocusedReadingAction]

    /**
      * Implicit wrapper that extends Seq implementations with random-related functionality
      * @param s
      * @tparam A
      */
    implicit class RandomizableSeq[A](s:Seq[A]) {
      /**
        * Fetches an element with a random index <b>with</b> replacement
        * @return A random member of the sequence
        */
      def randomElement:A = s(random.nextInt() % s.length)
    }
  }



}
