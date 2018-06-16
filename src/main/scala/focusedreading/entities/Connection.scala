package focusedreading.entities

import org.clulab.struct.Internalizer

/**
  * Created by enrique on 21/11/16.
  */
case class Connection(controller:Participant, controlled:Participant, sign:Boolean, evidence:Iterable[String], reference:Seq[String] = Seq()){

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Connection]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that:Connection => (this.controller== that.controller) && (this.controlled == that.controlled) && this.sign == that.sign
    case _ => false
  }

  private lazy val precomputedHash:Int = s"${controller.toString}:${controlled.toString}:$sign".hashCode

  override def hashCode(): Int = precomputedHash

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



object Connection {
  private val cache = new Internalizer[Connection]()

  def get(controller:Participant, controlled:Participant, sign:Boolean):Connection =
    cache.intern(Connection(controller, controlled, sign, Nil))
}