package focusedreading.executable.cross_validation

import focusedreading.agents.{LuceneIndexDir, PolicySearchAgent, SQLiteFile}
import focusedreading.entities.Participant
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import focusedreading.reinforcement_learning.states.FocusedReadingState
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.EpGreedyPolicy
import org.sarsamora.policy_iteration.td.value_functions.LinearApproximationActionValues
import org.sarsamora.policy_iteration.td.SARSA

class Trainer(dataSet:Iterator[Tuple2[(String, String), Array[String]]], indexPath:LuceneIndexDir, sqliteFile:SQLiteFile) {


  val focusedReadingFabric = () => {
    if(dataSet.hasNext){
      val (pair, sequence) = dataSet.next
      val participantA = Participant("", pair._1)
      val participantB = Participant("", pair._2)
      val referencePath = sequence map (p=> Participant("", p))

      // TODO: Integrate the normalization parameters
      Some(SimplePathEnvironment(participantA, participantB, referencePath, normalizationParameters = None)(indexPath, sqliteFile))
    }
    else
      None
  }

  def run():EpGreedyPolicy = {
    val policyIteration = new SARSA(focusedReadingFabric, 20000, 200, alpha = 0.05, gamma = 0.3, lambda = 1.0)
    val possibleActions = Set[Action]() ++ PolicySearchAgent.activeActions
    val qFunction = new LinearApproximationActionValues(possibleActions, FocusedReadingState.featureNames, true)
    val initialPolicy = new EpGreedyPolicy(0.5, qFunction)

    val learntPolicy = policyIteration.iteratePolicy(initialPolicy)


    learntPolicy.asInstanceOf[EpGreedyPolicy]
  }


}

