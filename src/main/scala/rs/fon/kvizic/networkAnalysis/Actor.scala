package rs.fon.kvizic.networkAnalysis

trait Actor {

	def relations: List[Mode]
	def updateRelations(newRels: List[Relation]): Actor
	def removeMode(relType: RelationType): Option[Actor]

	def getModeOrNone(relType: RelationType): Option[Mode] = {
		val modesOfType: List[Mode] = relations.filter(mode => mode.relType == relType)
		if (modesOfType.isEmpty) None
		else if (modesOfType.size > 1) throw new IllegalStateException("actor may have only one mode of any type")
		else Option[Mode](modesOfType.head)
	}

	def allRelTypes: List[RelationType] =
		for (mode <- relations) yield mode.relType

	def outDegree(relType: RelationType): Double = getModeOrNone(relType) match {
		case Some(mode) => mode.relations.foldLeft(0.0)((res, item) => res + item.weight)
		case None => 0
	}

	def getEndActorsOrNone(relType: RelationType): Option[List[Actor]] = getModeOrNone(relType) match {
		case Some(mode) => {
			val rels = mode.relations
			val listOfActors: List[Actor] = rels.map[Actor, List[Actor]] { rel => rel.endActor }
			Option[List[Actor]](listOfActors)
		}
		case None => None
	}


	def filterByRelType(relType: RelationType): Option[Actor] = relations match {
		case Nil => throw new IllegalStateException("Node without relations makes no sense!")
		case mode :: Nil => if (mode.relType == relType) Some(this) else None
		//recurse. if head is the one, just strip all the tails one by one. if not, remove head and see what head of the tail is
		case mode :: tail =>
			if (mode.relType == relType) {
				val removedMode2 = removeMode(tail.head.relType).get
				removedMode2.filterByRelType(relType)
			} else {
				val removedMode = removeMode(mode.relType).get
				removedMode.filterByRelType(relType)
			}

	}

	def getAllEndActors: List[Actor] = for {
		relType <- allRelTypes
		if getEndActorsOrNone(relType) != None
		endActor <- getEndActorsOrNone(relType).get
	} yield endActor

	def getAllRelations: List[Relation] = relations.foldLeft(List[Relation]())((allRels, mode2) => allRels ::: mode2.relations)

	//class that implements Actor trait is responsible for the relations order (ranking)
	def sync(relType: RelationType): Actor = {

		getModeOrNone(relType) match {
			case Some(mode) => {
				val rels = mode.relations
				def syncIter(counter: Int, rels: List[Relation], newRels: List[Relation]): List[Relation] = {
					if (rels.isEmpty) newRels
					else syncIter(counter + 1, rels.tail, syncRelation(rels.head, counter) :: newRels)
				}

				def syncRelation(rel: Relation, counter: Int): Relation =
					rel match {
						case r: RankRelation => r.updateWeight(rels.size, counter)
						case _ => rel
					}

				val newRels: List[Relation] = syncIter(0, rels, List[Relation]()).reverse
				updateRelations(newRels)
			}
			case None => this
		}

	}

	def addRelation(rel: Relation): Actor = {
		getModeOrNone(rel.relType) match {
			case Some(mode) => this updateRelations mode.addOrReplaceRelation(rel).relations
			case None => this updateRelations List[Relation](rel)
		}
	}

	def localClusteringCoef: Double = {
		val endActors = getAllEndActors
		if (endActors.size == 0 || endActors.size == 1) 0
		else {
			// endActorsConnections is a number of triadic closures.
			val endActorsConnections: Double = endActors.foldLeft(0)((acc, actor) => acc + actor.countHasRelsAmong(endActors.toSet))
			//returns number of triadic closures divided by the number of possible triadic closures
			endActorsConnections / (endActors.size * (endActors.size - 1))
		}
	}

	// counts the size of the intersection between the endActors of this actor and @param actors
	private def countHasRelsAmong(actors: Set[Actor]): Integer = {
		getAllEndActors.foldLeft(0)((acc, actor) => if (actors.contains(actor)) acc + 1 else acc)
	}
}