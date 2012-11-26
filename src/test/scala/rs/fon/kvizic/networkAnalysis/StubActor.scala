package rs.fon.kvizic.networkAnalysis

class StubActor(val name: String, likes: => List[_ >: StubActor]) extends Actor {

  override def toString: String = name

  def relations(): List[Mode] = {
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

  def updateRelations(newRels: List[Relation]): Actor = {
    val relType = newRels match {
      case List() => throw new IllegalArgumentException("Empty list doesn`t make sense")
      case head :: tail => head.relType
    }
    relType match {
      case LikeRelationType => updateLike(newRels)
      case unknownType => throw new IllegalArgumentException("Relation type " + unknownType.getClass() + " is not supported in Obin!")
    }
  }

  def updateLike(likes: List[Relation]): StubActor = {
    val actors: List[_ >: StubActor] = for (rel <- likes) yield rel match {
      case like: Like => like.endActor
      case _ => throw new IllegalArgumentException("unsupported relation")
    }
    new StubActor(name, actors)
  }

}

case object LikeRelationType extends RelationType("like")

class Like(paramEndActor: StubActor) extends BinaryRelation(paramEndActor) {
  def relType: RelationType = LikeRelationType
}
