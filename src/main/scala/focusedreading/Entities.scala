package org.clulab.reach.focusedreading

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.QueryParserBase
import org.clulab.odin.{EventMention, Mention}
import org.clulab.reach.PaperReader
import org.clulab.reach.grounding.{KBResolution, ReachKBUtils}
import org.clulab.reach.indexer.NxmlSearcher
//import org.clulab.reach.mentions.serialization.json.{JSONSerializer, REACHMentionSeq}
import org.clulab.reach.mentions.{BioMention, BioTextBoundMention, CorefEventMention, CorefMention, MentionOps}
import org.clulab.utils.Serializer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import scalax.collection.mutable.Graph // shortcuts

/**
  * Created by enrique on 21/11/16.
  */

case class Participant(namespace:String, id:String){
  lazy val synonyms =  Participant.dict.lift(id);

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Participant]

  override def equals(obj: Any): Boolean = obj match {
    case that:Participant => (this.namespace == that.namespace) && (this.id == that.id)
    case _ => false
  }

  override def hashCode(): Int =  (this.id + this.namespace).hashCode

  override def toString: String = s"$namespace:$id"
}

case class Connection(controller:Participant, controlled:Participant, sign:Boolean, evidence:Iterable[String], reference:Seq[String] = Seq()){

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Connection]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that:Connection => (this.controller== that.controller) && (this.controlled == that.controlled) && this.sign == that.sign
    case _ => false
  }

  override def hashCode(): Int = s"${controller.toString}:${controlled.toString}:$sign".hashCode

  def toString(humanFriendly:Boolean): String ={
    if(humanFriendly){

      val controllerLabel = controller.synonyms match {
        case Some(syns) => syns.head
        case None => controller.id
      }

      val controlledLabel = controlled.synonyms match {
        case Some(syns) => syns.head
        case None => controlled.id
      }

      s"Controller: $controllerLabel - Controlled: $controlledLabel - Sign: $sign"
    }
    else
      s"Controller: $controller - Controlled: $controlled - Sign: $sign"
  }

  override def toString: String = this.toString(false)


}

object Participant{
  var lines = ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("uniprot-proteins.tsv.gz")).getLines.toSeq
    lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("GO-subcellular-locations.tsv.gz")).getLines.toSeq
    lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz")).getLines.toSeq
    lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PubChem.tsv.gz")).getLines.toSeq
    lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("PFAM-families.tsv.gz")).getLines.toSeq
    lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("bio_process.tsv.gz")).getLines.toSeq
    lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("ProteinFamilies.tsv.gz")).getLines.toSeq
    lines ++= ReachKBUtils.sourceFromResource(ReachKBUtils.makePathInKBDir("hgnc.tsv.gz")).getLines.toSeq

    val dict = lines.map{ l => val t = l.split("\t"); (t(1), t(0)) }.groupBy(t=> t._1).mapValues(l => l.map(_._2).distinct)
}