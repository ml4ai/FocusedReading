package focusedreading.ir

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Paths

import com.typesafe.scalalogging.LazyLogging
//import org.clulab.processors.bionlp.BioNLPProcessor
import org.clulab.utils.StringUtils
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{IndexSearcher, TopScoreDocCollector}
import org.apache.lucene.store.FSDirectory
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Searches the NXML index created by NXML indexer
  * User: mihais. Altered by: Enrique
  * Date: 10/19/15
  */
class NxmlSearcher(val indexDir:String) extends LazyLogging {
  private val reader = DirectoryReader.open(FSDirectory.open(Paths.get(indexDir)))
  private val searcher = new IndexSearcher(reader)
  //private val proc = new BioNLPProcessor(withChunks = false)

  def close(): Unit = reader.close()

  def docs(ids:Seq[(Int, Float)]):Seq[(Document, Float)] = {

    ids map (id => (searcher.doc(id._1), id._2))

  }

  def searchByField(query:String,
                    field:String,
                    analyzer:Analyzer,
                    totalHits:Option[Int],
                    verbose:Boolean = true):Set[(Int, Float)] = {
    try{
      val q = new QueryParser(field, analyzer).parse(query)
      val collector =totalHits match {
        case Some(i) => TopScoreDocCollector.create(totalHits.get)
        case None => TopScoreDocCollector.create(10000) // TODO: Do this cleanly
      }

      searcher.search(q, collector)
      val hits = collector.topDocs().scoreDocs
      val results = new mutable.HashSet[(Int, Float)]
      for(hit <- hits) {
        val docId = hit.doc
        val score = hit.score
        results += new Tuple2(docId, score)
      }
      if(verbose) logger.debug(s"""Found ${results.size} results for query "$query"""")
      results.toSet
    }catch{
      case _:Throwable => Set()
    }

  }



}


