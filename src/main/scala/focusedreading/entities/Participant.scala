package focusedreading.entities

import focusedreading.KnowledgeBases
import org.clulab.reach.grounding.ReachKBUtils
import org.clulab.struct.Internalizer

case class Participant(namespace:String, id:String){

  lazy val synonyms =  KnowledgeBases.dict.lift(id)

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Participant]

  override def equals(obj: Any): Boolean = obj match {
    case that:Participant => (this.namespace == that.namespace) && (this.id == that.id)
    case _ => false
  }

  private lazy val precomputedHashCode = (this.id + this.namespace).hashCode

  override def hashCode(): Int =  precomputedHashCode

  override def toString: String = s"$namespace:$id"
}

object Participant{
  private val cache = new Internalizer[Participant]()

  def get(id:String, value:String): Participant = cache.intern(Participant(id, value))
}
