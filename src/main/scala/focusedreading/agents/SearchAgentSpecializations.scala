package focusedreading.agents

import focusedreading._
import focusedreading.entities.Participant
import focusedreading.ie.SQLIteIEStrategy
import focusedreading.ir.queries.QueryStrategy._
import focusedreading.ir.LuceneIRStrategy
import focusedreading.ir.queries.Query
import focusedreading.search_models._
import focusedreading.pc_strategies.MostConnectedParticipantsStrategy

/*
 * Created by enrique on 18/02/17.
 *
 * Mixes traits together to implement FR search agents
 */

case class LuceneIndexDir(path:String)
case class SQLiteFile(path:String)


/**
  * Uses Redis for IR and SQLite for IE to do simple path FR
  * @param participantA Origin of the search
  * @param participantB Destination of the search
  */
class RedisSQLiteSearchAgent(participantA:Participant, participantB:Participant)(implicit indexPath:LuceneIndexDir, sqliteFile:SQLiteFile) extends SimplePathAgent(participantA, participantB)
  with MostConnectedParticipantsStrategy
  with LuceneIRStrategy
  with SQLIteIEStrategy {

  override val indexDir:String = indexPath.path
  override val sqlitePath: String = sqliteFile.path


  override def choseQuery(source: Participant,
                          destination: Participant,
                          model: SearchModel) = Query(Cascade, 200, source, Some(destination)) //TODO: Fix this to a more elegant way of ignoring the retrieval count

}


