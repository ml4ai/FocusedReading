package focusedreading.executable.cross_validation

import java.io.{File, FileOutputStream, OutputStreamWriter}

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import focusedreading.Configuration
import focusedreading.agents.{LuceneIndexDir, SQLiteFile}

import scala.collection.mutable

/**
  * Created by enrique on 07/08/17.
  */
object CrossVal extends App with LazyLogging {

  val indexPath = Configuration.Lucene.indexPath
  val sqlitePath = Configuration.SQLite.dbPath

  val dirPath = args(0)

  val dir = new File(dirPath)

  val foldFiles = dir.listFiles().filter(f => f.getName.toLowerCase.endsWith(".tsv"))

  val slices = makeCVSlices(foldFiles)


  // CV results
  val numPathsFound = new mutable.ArrayBuffer[Int]()
  val numPapersRead = new mutable.ArrayBuffer[Int]()
  val numQueriesIssued = new mutable.ArrayBuffer[Int]()

  for((trainData, testData) <- slices){
    // Do training
    val trainer = new Trainer(trainData.toIterator, LuceneIndexDir(indexPath), SQLiteFile(sqlitePath))
    val learntPolicy = trainer.run()
    // Do testing
    val tester = new Tester(testData map (_._1), learntPolicy)
    val (numFound, papersRead, queriesIssued) = tester.run()
    // Collect results
    numPathsFound += numFound
    numPapersRead += papersRead
    numQueriesIssued += queriesIssued
  }

  // Print stats
  val pathsStats = stats(numPathsFound)
  val papersStats = stats(numPapersRead)
  val queriesStats = stats(numQueriesIssued)

  saveData("explore_baseline.csv.prefix", numPathsFound, numPapersRead, numQueriesIssued)

  println()
  println(s"Paths Found\tMean:${pathsStats._1}\tStd Err: ${pathsStats._2}")
  println(s"Papers Read\tMean:${papersStats._1}\tStd Err: ${papersStats._2}")
  println(s"Queries Issued\tMean:${queriesStats._1}\tStd Err: ${queriesStats._2}")
  println()

  def saveData(name:String, paths:Seq[Int], papers:Seq[Int], queries:Seq[Int]) = {
    val lines = (0 until paths.size).map{
      ix =>
        val (p, pp, q) = (paths(ix), papers(ix), queries(ix))
        s"$p,$pp,$q\n"
    }

    val ow = new OutputStreamWriter(new FileOutputStream(name))
    lines foreach ow.write
    ow.close
  }

  def stats(values:Seq[Int]):(Double, Double) = {
    val mean = values.sum / values.size.toDouble
    val serr = Math.sqrt((values.map{_ - mean}.map{ v => Math.pow(v, 2)}.sum)/  (values.size - 1))

    (mean, serr)
  }

  // TODO: Check the return types, perhaps replace it by a case class for simplification
  def makeCVSlices(foldFiles:IndexedSeq[File]):List[(List[((String, String), Array[String])], List[((String, String), Array[String])])] = {
    val folds = foldFiles map {
      file =>
        io.Source.fromFile(file).getLines().toList.map{
          l =>
            val x = l.split('\t')
            ((x.head, x.last), x)
        }
    }

    val indices = 0.until(foldFiles.size).toList

    val slices = indices map {
      ix =>
        val test = folds(ix)
        val train = indices.filter(_ != ix).flatMap(folds)

        (train, test)
    }

    slices
  }
}
