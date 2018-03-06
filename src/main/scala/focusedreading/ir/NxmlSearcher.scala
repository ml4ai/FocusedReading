package focusedreading.ir

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Paths

import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.bionlp.BioNLPProcessor
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
  val reader = DirectoryReader.open(FSDirectory.open(Paths.get(indexDir)))
  val searcher = new IndexSearcher(reader)
  val proc = new BioNLPProcessor(withChunks = false)

  def close() = reader.close()

  def docs(ids:Seq[(Int, Float)]):Seq[(Document, Float)] = {

    ids map (id => (searcher.doc(id._1), id._2))

  }



  def search(query:String, totalHits:Int):Set[(Int, Float)] = {
    searchByField(query, "text", new StandardAnalyzer(), totalHits)
  }

  def searchId(id:String):Set[(Int, Float)] = {
    searchByField(id, "id", new WhitespaceAnalyzer(), 1)
  }

  def searchByField(query:String,
                    field:String,
                    analyzer:Analyzer,
                    totalHits:Int,
                    verbose:Boolean = true):Set[(Int, Float)] = {
    try{
      val q = new QueryParser(field, analyzer).parse(query)
      val collector = TopScoreDocCollector.create(totalHits)
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

  def intersection(s1:Set[(Int, Float)], s2:Set[(Int, Float)]):Set[(Int, Float)] = {
    val result = new mutable.HashSet[(Int, Float)]()
    for(s <- s1) {
      var found = false
      var otherScore = 0.0.toFloat
      for(o <- s2 if ! found) {
        if(s._1 == o._1) {
          found = true
          otherScore = o._2
        }
      }
      if(found) {
        result += new Tuple2(s._1, s._2 + otherScore)
      }
    }
    result.toSet
  }

  def union(s1:Set[Int], s2:Set[Int]):Set[Int] = {
    val result = new mutable.HashSet[Int]()
    s1.foreach(result += _)
    s2.foreach(result += _)
    result.toSet
  }


}


