package rs.fon.kvizic.networkAnalysis

class Network(val actors: List[Actor] = List[Actor]()) {

  protected def actorsByClass: Map[Class[_ <: Actor], List[Actor]] = actors.groupBy[Class[_ <: Actor]](actor => actor.getClass())

  def removeActor(oldActor: Actor): Option[Network] = actors.filter(a => !(a == oldActor)) match {
    case List() => None
    case newList => Option(new Network(newList))
  }

  def removeActors(oldActors: List[Actor]): Option[Network] = {

    def removeActorsIter(networkOption: Option[Network], actor: Actor): Option[Network] = {
      networkOption match {
        case None => None
        case Some(network) => network removeActor actor
      }
    }

    oldActors.foldLeft(Option(this))(removeActorsIter)
  }

  def addOrReplaceActor(newActor: Actor): Network = {
    val actorsAfterRemove: List[Actor] = removeActor(newActor) match {
      case None => List[Actor]()
      case Some(networkAfterRemove) => networkAfterRemove.actors
    }
    new Network(newActor :: actorsAfterRemove)

  }
  def addOrReplaceActors(newActors: List[Actor]): Network = newActors.foldLeft(this)((network, actor) => network addOrReplaceActor actor)

  def outDegrees(relType: RelationType): Map[Actor, Int] = {
    val noOfOutgoingRelsList: List[(Actor, Int)] =
      for (actor: Actor <- actors if actor.outDegree(relType) > 0)
        yield (actor, actor.outDegree(relType))
    noOfOutgoingRelsList toMap
  }

  def inDegrees(relType: RelationType): Map[Actor, Int] = {

    def inDegreesActorIter(inDegrees: Map[Actor, Int], actor: Actor): Map[Actor, Int] = {

      def inDegreesEndActorIter(inDegreesInput: Map[Actor, Int], endActor: Actor): Map[Actor, Int] =
        if (inDegreesInput contains endActor)
          inDegreesInput updated (endActor, inDegreesInput(endActor) + 1)
        else inDegreesInput + (endActor -> 1)

      actor.getEndActorsOrNone(relType) match {
        case Some(endActors) => {
          endActors.foldLeft(inDegrees)(inDegreesEndActorIter)
        }
        case None => inDegrees
      }
    }

    actors.foldLeft(Map[Actor, Int]())(inDegreesActorIter)
  }
}

