package focusedreading.executable.cross_validation

import focusedreading.agents.PolicySearchAgent
import focusedreading.Participant
import focusedreading.reinforcement_learning.environment.SimplePathEnvironment
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, LinearApproximationValues}
import org.sarsamora.policy_iteration.td.SARSA

class Trainer(dataSet:Iterator[Tuple2[(String, String), Array[String]]]) {


  def focusedReadingFabric():Option[Environment] = {
    if(dataSet.hasNext){
      val (pair, sequence) = dataSet.next
      val participantA = Participant("", pair._1)
      val participantB = Participant("", pair._2)
      val referencePath = sequence map (p=> Participant("", p))

      // TODO: Integrate the normalization parameters
      Some(new SimplePathEnvironment(participantA, participantB, referencePath, normalizationParameters = None))
    }
    else
      None
  }

  def run():EpGreedyPolicy = {
    val policyIteration = new SARSA(focusedReadingFabric, 20000, 200, alpha = 0.05, gamma = 0.3)
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

