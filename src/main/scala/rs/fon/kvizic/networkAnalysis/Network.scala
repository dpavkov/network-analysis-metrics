package rs.fon.kvizic.networkAnalysis

import rs.fon.kvizic.networkAnalysis.algorithm.Tarjan

class Network(val actors: List[Actor] = List[Actor]()) {
  
  override def toString: String = "\nNetwork: " + {
    for (actor <- actors) yield "\n" + actor + ": " + actor.getAllEndActors
  }

  protected[networkAnalysis] def actorsByClass: Map[Class[_ <: Actor], List[Actor]] = actors.groupBy[Class[_ <: Actor]](actor => actor.getClass())

  def removeActor(oldActor: Actor): Option[Network] = actors.filter(a => !(a == oldActor)) match {
    case List() => None
    case newList => Option(new Network(newList))
  }
  
  def stronglyConnectedComponents: List[Network] = Tarjan.connectedComponents(this)
  
  def stronglyConnectedComponents(relType: RelationType): List[Network] = 
    this.filterByRelType(relType) match {
      case None => List()
      case Some(network) => {
        Tarjan.connectedComponents(network)
      }
    }
  
  def averageDegree(relType: RelationType): Double = {
    val outDegreesMap = outDegrees(relType)
    outDegreesMap.foldLeft(0.0)((acc, actorEntry) => acc + actorEntry._2) / actors.size
  }

  def filterByRelType(relType: RelationType): Option[Network] = {
    //for each actor, filter end actor. 
    def filterEndActors(actor: Actor, newNetwork: List[Actor]): Actor = {

      def getActor(relation: Relation, fromActors: List[Actor]): Relation = {
        if (fromActors.isEmpty) relation
        else if (fromActors.head == relation.endActor) relation.updateEndActor(fromActors.head)
        else getActor(relation, fromActors.tail)
      }

      val filteredEndActors: List[Relation] =
        // By this point, all the actors are one-mode actors so there is no need to check if mode is none. However, this means that this function is not generic and should not be extracted outside of filterByRelType without refactoring!
        for {
          relation <- actor.getModeOrNone(relType).get.relations
        } yield getActor(relation, newNetwork)

      actor.updateRelations(filteredEndActors)
    }

    val filteredActors: List[Actor] =
      actors.foldLeft(List[Actor]())((filtered, actor) =>
        actor.filterByRelType(relType) match {
          case None => filtered
          case Some(actor) => actor :: filtered
        })
    if (filteredActors.isEmpty) None
    else {
      val filteredEndActors: List[Actor] =
        for (actor <- filteredActors) yield filterEndActors(actor, filteredActors)
      Some(new Network(filteredEndActors))
    }
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

  def outDegrees(relType: RelationType): Map[Actor, Double] = {
    val noOfOutgoingRelsList: List[(Actor, Double)] =
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

