package focusedreading.supervision

/**
  * Segment (or complete) reference path for supervision and evaluation purposes
  * @param source Initial entity of the reference path
  * @param destination Final entity of the reference path
  * @param elemSeq All the entities comprising the reference path
  */
case class ReferencePathSegment(source:String, destination:String, elemSeq:Seq[String])
