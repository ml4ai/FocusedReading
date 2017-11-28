package focusedreading.executable.cross_validation

import org.clulab.focusedreading.agents.PolicySearchAgent
import org.clulab.reach.focusedreading.Participant
import org.clulab.reach.focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, LinearApproximationValues}
import org.sarsamora.policy_iteration.td.SARSA

class Trainer(dataSet:Iterator[Tuple2[String, String]]) {


  def focusedReadingFabric():Option[Environment] = {
    if(dataSet.hasNext){
      val episode = dataSet.next
      val participantA = Participant("", episode._1)
      val participantB = Participant("", episode._2)

      Some(new SimplePathEnvironment(participantA, participantB))
    }
    else
      None
  }

  def run():EpGreedyPolicy = {
    val policyIteration = new SARSA(focusedReadingFabric, 20000, 200, alpha = 0.05, gamma = 0.3)
    // TODO: Put the action choice on a better place
    val possibleActions = Set[Action]() ++ PolicySearchAgent.usedActions
    val qFunction = new LinearApproximationValues(possibleActions)
    val initialPolicy = new EpGreedyPolicy(0.5, qFunction)

    val learntPolicy = policyIteration.iteratePolicy(initialPolicy)

    // Store the policy somewhere
    // Serializer.save(learntPolicy, "learnt_policy.ser")
    //learntPolicy.save("learnt_policy.json")

    //    val f = Figure()
    //    val p = f.subplot(0)
    //    val x = linspace(0.0, policyIteration.controlCount.toDouble, policyIteration.controlCount)
    //
    //    val num = qFunction.coefficientsExplore.size
    //    val names = qFunction.coefficientsExplore.keySet.toSeq.sorted
    //    for(i <- 0 until num) {
    //      val history = DenseVector(qFunction.coefficientMemoryExplore.map {
    //        v =>
    //          if(v.length == 0)
    //            0.0
    //          else
    //            v(i)
    //      }.toArray)
    //
    //      p += plot(x, history, '-', null, names(i))
    //    }
    //
    //    p.legend = true
    //    p.xlabel = "Update #"
    //    p.ylabel = "Coef Explore value"
    //
    //    f.saveas("plot_explore.png")

    learntPolicy.asInstanceOf[EpGreedyPolicy]
  }


}

