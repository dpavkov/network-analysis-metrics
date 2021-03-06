package rs.fon.kvizic.networkAnalysis
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.ShortestPath
import rs.fon.kvizic.networkAnalysis.algorithm.centrality.Centrality
import rs.fon.kvizic.networkAnalysis.algorithm.centrality.BonacichVector
import rs.fon.kvizic.networkAnalysis.algorithm.centrality.BonacichCentrality

class Network(val actors: List[Actor] = List[Actor]()) {

	override def toString: String = "\nNetwork: " + {
		for (actor <- actors) yield "\n" + actor + ": " + actor.getAllEndActors
	}

	protected[networkAnalysis] def actorsByClass: Map[Class[_ <: Actor], List[Actor]] = actors.groupBy[Class[_ <: Actor]](actor => actor.getClass())

	private val dataHolder = new DataHolder(this)

	def removeActor(oldActor: Actor): Option[Network] = actors.filter(a => !(a == oldActor)) match {
		case List() => None
		case newList => Option(new Network(newList))
	}

	def stronglyConnectedComponents: List[Network] = dataHolder.connectedComponents

	def centrality: Map[Actor, Double] = dataHolder.centralityValues

	def betweenness: Map[Actor, Double] = dataHolder.betweennessValues

	def brokerage: Map[Actor, Double] = dataHolder.brokerage

	def getBonacichCentrlity(vector: BonacichVector, degree: Int) = {

		def bonacichIter(centrality: BonacichCentrality, iter: Int): BonacichCentrality = {
			if (iter > 0)
				bonacichIter(centrality.next(vector), iter - 1)
			else {
				dataHolder.bonacich = centrality;
				centrality
			}
		}

		bonacichIter(dataHolder.bonacich, degree - dataHolder.bonacich.currentIter(vector)).iterations(vector)(degree - 1)

	}
	
	def neighborhoodOverlap: Map[Actor, Map[Actor, Double]] = dataHolder.neighborhoodOverlapValues
	
	def neighborhoodOverlapFor(from: Actor, to: Actor): Double = 
	  dataHolder.neighborhoodOverlapValues.get(from) match {
	    case None => 0
	    case Some(fromValues) => fromValues.getOrElse(to, 0)
	  }
	

	def stronglyConnectedComponents(relType: RelationType): List[Network] =
		this.filterByRelType(relType) match {
			case None => List()
			case Some(network) => {
				stronglyConnectedComponents
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

	def inDegrees(relType: RelationType): Map[Actor, Double] = {

		// increases @param inDegrees map for all @param actor's relations
		def inDegreesActorIter(inDegrees: Map[Actor, Double], actor: Actor): Map[Actor, Double] = {

			def inDegreesEndActorIter(inDegreesInput: Map[Actor, Double], relation: Relation): Map[Actor, Double] = {
			  val endActor = relation.endActor
			  if (inDegreesInput contains endActor)
				  	// if this actor is already there, increases existing in degrees by weight
					inDegreesInput updated (endActor, inDegreesInput(endActor) + relation.weight)
				// otherwise, adds the actor to the map
				else inDegreesInput + (endActor -> relation.weight)
			}

			actor.getModeOrNone(relType) match {
				case Some(mode) => {
					mode.relations.foldLeft(inDegrees)(inDegreesEndActorIter)
				}
				case None => inDegrees
			}
		}
		//starts with the empty in degrees map, and iterates through the actors
		actors.foldLeft(Map[Actor, Double]())(inDegreesActorIter)
	}
}

