package rs.fon.kvizic.networkAnalysis.algorithm.neighborhoodOverlap

import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.Network

class NeighborhoodOverlap (val network: Network) {

	// returns neighborhood overlap for each relation in the network
	def overlap(): Map[Actor, Map[Actor, Double]] = {
	 val coeff = for (actor: Actor <- network.actors) 
	    yield (actor -> calculateCoefficient(actor))
	 coeff toMap
	}
  
	//calculates neighborhood overlap for each relation of @param actor
	private def calculateCoefficient(actor: Actor): Map[Actor, Double] = {

		def packResults(resultsIter: (Actor, Option[Double])): List[(Actor, Double)] = resultsIter._2 match {
			case None => List()
			case Some(value) => List(resultsIter._1 -> value)
		}
		val endActors = actor.getAllEndActors
		// for each end actor
		val results: List[(Actor, Option[Double])] = for (endActor <- endActors)
			//do the calculation
			yield (endActor, calculateCoefficientFor(endActors.filter(_ != endActor), endActor.getAllEndActors.filter(_ != actor)))
		//and pack the results in the appropriate structure
		results.flatMap(packResults).toMap
	}

	type Coefficient = (Int, Int)

	// calculates coefficient given relations of both actors
	private def calculateCoefficientFor(myEndActors: List[Actor], hisEndActors: List[Actor]): Option[Double] = {
		if (myEndActors.isEmpty || hisEndActors.isEmpty) None
		else {
			val result: Coefficient = myEndActors.foldLeft(
				new Coefficient(0, hisEndActors.length))((coef, actor) =>
					if (hisEndActors.contains(actor))
						new Coefficient(coef._1 + 1, coef._2)
					else new Coefficient(coef._1, coef._2 + 1))
			Some(result._1.toDouble / result._2)
		}
	}

}