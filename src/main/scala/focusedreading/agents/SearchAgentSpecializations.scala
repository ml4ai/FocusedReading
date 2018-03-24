package focusedreading.agents

import java.io.{FileOutputStream, OutputStreamWriter}

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.pc_strategies.{MostConnectedParticipantsStrategy, PolicyParticipantsStrategy}
import focusedreading.ie.{REACHIEStrategy, SQLIteIEStrategy}
import focusedreading.ir.QueryStrategy._
import focusedreading.ir.{LuceneIRStrategy, Query, QueryStrategy, RedisIRStrategy, SQLIRStrategy}
import focusedreading.models._
import focusedreading.reinforcement_learning.actions._
import focusedreading.reinforcement_learning.states.{FocusedReadingState, RankBin}
import org.sarsamora.actions.Action
import org.sarsamora.policies.Policy
import org.sarsamora.states.State
import focusedreading._
import focusedreading.agents._
import focusedreading.agents.FocusedReadingStage._


import scala.collection.mutable

/*
 * Created by enrique on 18/02/17.
 *
 * Mixes traits together to implement FR search agents
 */

/**
  * Uses Lucene and REACH directly to do a FR simple path search
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  */
class LuceneReachSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with LuceneIRStrategy
  with REACHIEStrategy {

  // Graph4Scala model
  /*override val */model/*:SearchModel*/ = new GFSModel(participantA, participantB) // Directed graph with the model.


  // Follow the cascade query strategy
  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, 50, source, Some(destination)) //TODO: Fix this to a more elegant way of ignoring the retrieval count


}

/**
  * Uses Redis for IR and SQLite for IE to do simple path FR
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  */
class RedisSQLiteSearchAgent(participantA:Participant, participantB:Participant) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with LuceneIRStrategy
  with SQLIteIEStrategy {


  /*override val */model/*:SearchModel*/ = new GFSModel(participantA, participantB) // Directed graph with the model.



  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, 200, source, Some(destination)) //TODO: Fix this to a more elegant way of ignoring the retrieval count

}


