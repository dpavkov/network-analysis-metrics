package rs.fon.kvizic.networkAnalysis.algorithm.neighborhoodOverlap

import rs.fon.kvizic.networkAnalysis.Actor

class NeighborhoodOverlap {

	def calculateCoefficient(actor: Actor): Map[Actor, Double] = {

		def packResults(resultsIter: (Actor, Option[Double])): List[(Actor, Double)] = resultsIter._2 match {
			case None => List()
			case Some(value) => List(resultsIter._1 -> value)
		}

		val endActors = actor.getAllEndActors
		val results: List[(Actor, Option[Double])] = for (endActor <- endActors)
			yield (endActor, calculateCoefficientFor(endActors.filter(_ != endActor), endActor.getAllEndActors.filter(_ != actor)))
		results.flatMap(packResults).toMap
	}

	type Coefficient = (Int, Int)

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