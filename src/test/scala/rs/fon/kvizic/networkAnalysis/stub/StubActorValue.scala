package rs.fon.kvizic.networkAnalysis.stub

import rs.fon.kvizic.networkAnalysis.RelationType
import rs.fon.kvizic.networkAnalysis.ValuedRelation
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.Relation
import rs.fon.kvizic.networkAnalysis.Mode

class StubActorValue(val name: String, likes: => Map[_ >: Actor, Double]) extends Actor {
  
    override def toString: String = name

  def relations(): List[Mode] = {
    if (likes.isEmpty) List.empty
    else {
      val relations: List[LikeValue] = (for (actor <- likes) yield {
        actor match {
          case (stub: Actor, value: Double) => new LikeValue(value, stub)
          case _ => throw new IllegalArgumentException("relation not allowed")
        }
      }) toList
      val likeMode = new Mode(relations, LikeValueRelationType)
      likeMode :: Nil
    }

  }

  def updateRelations[A >: Actor](newRels: List[Relation]): A = {
    val relType = newRels match {
      case List() => throw new IllegalArgumentException("Empty list doesn`t make sense")
      case head :: tail => head.relType
    }
    relType match {
      case LikeValueRelationType => updateLike(newRels)
      case unknownType => throw new IllegalArgumentException("Relation type " + unknownType.getClass() + " is not supported in Obin!")
    }
  }

  def updateLike(likes: List[Relation]): StubActor = {
    val actors: List[_ >: StubActor] = for (rel <- likes) yield rel match {
      case like: LikeValue => like.endActor
      case _ => throw new IllegalArgumentException("unsupported relation")
    }
    new StubActor(name, actors)
  }
  
   def removeMode(relType: RelationType): Option[Actor] =
     if (relType == LikeValueRelationType) None
     else Some(this)

}

case object LikeValueRelationType extends RelationType("likeValued")

class LikeValue(paramWeight: Double, paramEndActor: Actor) extends ValuedRelation(paramWeight, paramEndActor) {
  def updateEndActor(endActor: Actor): Relation = new LikeValue (paramWeight, endActor)
  def relType: RelationType = LikeValueRelationType
}