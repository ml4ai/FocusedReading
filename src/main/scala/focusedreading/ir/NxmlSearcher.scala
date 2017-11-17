package org.clulab.reach.focusedreading.ir

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
  * User: mihais
  * Date: 10/19/15
  */
class NxmlSearcher(val indexDir:String) extends LazyLogging {
  val TOTAL_HITS = 200
  val reader = DirectoryReader.open(FSDirectory.open(Paths.get(indexDir)))
  val searcher = new IndexSearcher(reader)
  val proc = new BioNLPProcessor(withChunks = false)

  def close() = reader.close()

  def docs(ids:Seq[(Int, Float)]):Seq[(Document, Float)] = {

    ids map (id => (searcher.doc(id._1), id._2))

  }

  def saveIds(docs:Set[(Document, Float)]): Unit = {
    val os = new PrintWriter(new FileWriter("ids.txt"))
    for(doc <- docs) {
      val id = doc._1.get("id")
      os.println(id)
    }
    os.close()
  }

  def saveNxml(resultDir:String, docs:Set[(Document, Float)], howManyToSave:Int = 0): Unit = {
    val docSeq = if (howManyToSave > 0) {
      docs.toSeq.sortBy(-_._2).take(howManyToSave)
    } else {
      docs.toSeq.sortBy(-_._2)
    }
    val sos = new PrintWriter(new FileWriter(resultDir + File.separator + "scores.tsv"))
    for(doc <- docSeq) {
      val id = doc._1.get("id")
      val nxml = doc._1.get("nxml")
      val os = new PrintWriter(new FileWriter(resultDir + File.separator + id + ".nxml"))
      os.print(nxml)
      os.close()
      sos.println(s"$id\t${doc._2}")
    }
    sos.close()
  }

  def saveDocs(resultDir:String, docIds:Set[(Int, Float)]): Unit = {
    val sos = new PrintWriter(new FileWriter(resultDir + File.separator + "scores.tsv"))
    var count = 0
    for(docId <- docIds) {
      val doc = searcher.doc(docId._1)
      val id = doc.get("id")
      val nxml = doc.get("nxml")
      val year = doc.get("year")
      val size = nxml.toString.length * 2 // in bytes
      val os = new PrintWriter(new FileWriter(resultDir + File.separator + id + ".nxml"))
      os.print(nxml)
      os.close()
      sos.println(s"$id\t${docId._2}\t$year\t$size")
      count += 1
    }
    sos.close()
    logger.info(s"Saved $count documents.")
  }

  def search(query:String, totalHits:Int = TOTAL_HITS):Set[(Int, Float)] = {
    searchByField(query, "text", new StandardAnalyzer(), totalHits)
  }

  def searchId(id:String, totalHits:Int = 1):Set[(Int, Float)] = {
    searchByField(id, "id", new WhitespaceAnalyzer(), totalHits)
  }

  def searchByField(query:String,
                    field:String,
                    analyzer:Analyzer,
                    totalHits:Int = TOTAL_HITS,
                    verbose:Boolean = true):Set[(Int, Float)] = {
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

  def countDocsContaining(eventDocs:Set[(Int, Float)], token:String):Int = {
    // token could be a phrase; make sure quotes are used
    val query = s"""Ras AND "$token""""
    val result = intersection(eventDocs, search(query))
    result.size
  }


}


