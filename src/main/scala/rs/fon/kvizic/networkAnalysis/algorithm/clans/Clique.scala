package rs.fon.kvizic.networkAnalysis.algorithm.clans

import rs.fon.kvizic.networkAnalysis.Actor

class Clique {

	def isA(actors: List[Actor]): Boolean = {
		val connected: List[Boolean] = for (actor <- actors; connects <- actors)
			yield (actor == connects) || actor.getAllEndActors.contains(connects);

		connected.foldLeft(true)((acc, current) => acc && current)
	}
}