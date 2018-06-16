package focusedreading.sqlite

import java.io.File
import java.sql._

import com.typesafe.scalalogging.LazyLogging
import jdk.internal.org.objectweb.asm.util.Printer
import focusedreading.ir.LuceneIRStrategy
import focusedreading.ir.queries.Query
import focusedreading.{_}

import scala.util.{Failure, Success, Try}

/**
  * Class that encapsulates the SQLite access related to Lucene queries
  * Created by enrique on 21/02/17.
  */


class LuceneDataAccess(sqlitePath:String, val indexDir:String) extends LazyLogging with LuceneIRStrategy{

  // Load the JDBC driver
  Class.forName("org.sqlite.JDBC")

  // DDL Commands
  val queryTable =
    """ CREATE TABLE Queries (
      |   id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      |   pa TEXT NOT NULL,
      |   pb TEXT,
      |   type TEXT NOT NULL,
      |   UNIQUE (pa, pb, type)
    | );""".stripMargin

  val queryResultTable =
    """ CREATE TABLE QueryResults (
      |   queryid INTEGER NOT NULL,
      |   pmcid TEXT NOT NULL,
      |   FOREIGN KEY(queryid) REFERENCES Queries(id),
      |   PRIMARY KEY(queryid, pmcid)
    | );""".stripMargin

  val ddlStatements = Seq(queryTable, queryResultTable)
  ////////////////


  private def getConnection = DriverManager.getConnection(s"jdbc:sqlite:$sqlitePath");

  def createDatabase(): Unit ={

    logger.info(s"Creating database in $sqlitePath")
    val connection:Connection = getConnection

    // Create all the tables
    for(command <- ddlStatements){
      val statement:Statement = connection.createStatement()
      statement.executeUpdate(command)
      statement.close
    }

    connection.close
    logger.info("Finished creating database")
  }

  val insertQueryCommand = "INSERT INTO Queries(pa, pb, type) VALUES(?, ?, ?);"
  val insertQueryResults = "INSERT INTO QueryResults(queryid, pmcid) VALUES(?, ?);"

  def insert(q:Query): Unit ={

    val conn = getConnection

    val selectStatement = q.B match {
      case Some(b) => conn.prepareStatement("SELECT id FROM Queries WHERE pa = ? AND pb = ? AND type = ?")
      case None => conn.prepareStatement("SELECT id FROM Queries WHERE pa = ? AND pb IS NULL AND type = ?")
    }

    selectStatement.setString(1, q.A.id)

    q.B match {
      case Some(b) =>
        selectStatement.setString(2, b.id)
        selectStatement.setString(3, q.strategy.toString)
      case None =>
        selectStatement.setString(2, q.strategy.toString)
    }

    // Only continue if this result set is empty
    val rs = selectStatement.executeQuery
    if(!rs.next()){
      // Test wether the record already exists
      // Execute the lucene query to get the results
      val resultsTry = Try(this.informationRetrieval(q))

      resultsTry match {
        case Success(results) => {


          try{
            val statement = conn.prepareStatement(insertQueryCommand)
            statement.setString(1, q.A.id)

            q.B match {
              case Some(b) =>
                statement.setString(2, b.id)
              case None =>
                statement.setNull(2, java.sql.Types.VARCHAR)
            }

            statement.setString(3, q.strategy.toString)
            statement.executeUpdate

            // Get the qid
            val x = conn.prepareStatement("SELECT last_insert_rowid();");
            val rs = x.executeQuery()
            rs.next
            val qid = rs.getInt(1)

            val statement2 = conn.prepareStatement(insertQueryResults)
            for(r <- results){
              statement2.setInt(1, qid)
              statement2.setString(2, r._1)
              statement2.addBatch
            }

            try{
              conn.setAutoCommit(false)
              statement2.executeBatch
              conn.setAutoCommit(true)
            }catch {
              case e:Exception =>
                logger.error(s"Problem storing results for $q")
            }

          } catch {
            case e:Exception =>
              logger.error(s"Problem storing results for $q")
          }



        }
        case Failure(e) => logger.error(s"Error doing information retrival for $q")

      }
    }

    conn.close

  }




}
