package rs.fon.kvizic.networkAnalysis.stub

import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.BinaryRelation
import rs.fon.kvizic.networkAnalysis.Mode
import rs.fon.kvizic.networkAnalysis.Relation
import rs.fon.kvizic.networkAnalysis.RelationType
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.Mode
import rs.fon.kvizic.networkAnalysis.Relation
import rs.fon.kvizic.networkAnalysis.RelationType

class StubActor(val name: String, likes: => List[_ >: StubActor]) extends Actor {

  override def toString: String = name

  override def relations(): List[Mode] = {
    if (likes.isEmpty) List.empty
    else {
      val relations: List[Like] = (for (actor <- likes) yield {
        actor match {
          case stub: StubActor => new Like(stub)
          case _ => throw new IllegalArgumentException("relation not allowed")
        }
      }) toList
      val likeMode = new Mode(relations, LikeRelationType)
      likeMode :: Nil
    }

  }

  override def updateRelations[A >: Actor](newRels: List[Relation]): A = {
    val relType = newRels match {
      case List() => throw new IllegalArgumentException("Empty list doesn`t make sense")
      case head :: tail => head.relType
    }
    relType match {
      case LikeRelationType => updateLike(newRels)
      case unknownType => throw new IllegalArgumentException("Relation type " + unknownType.getClass() + " is not supported in Obin!")
    }
  }

  def updateLike[A >: Actor](likes: List[Relation]): A = {
    val actors: List[_ >: StubActor] = for (rel <- likes) yield rel match {
      case like: Like => like.endActor
      case _ => throw new IllegalArgumentException("unsupported relation")
    }
    new StubActor(name, actors)
  }
  
   def removeMode(relType: RelationType): Option[Actor] =
     if (relType == LikeRelationType) None
     else Some(this)

}

case object LikeRelationType extends RelationType("like")

class Like(paramEndActor: Actor) extends BinaryRelation(paramEndActor) {
  def updateEndActor(endActor: Actor): Relation = new Like(endActor)
  def relType: RelationType = LikeRelationType
}
