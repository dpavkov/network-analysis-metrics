package rs.fon.kvizic.networkAnalysis.algorithm.centrality

import rs.fon.kvizic.networkAnalysis.Actor

class BonacichCentrality(val firstCalc: Map[Actor, Double], val iterations: Map[BonacichVector, List[Map[Actor, Double]]]) {

	def next(vector: BonacichVector): BonacichCentrality =
		new BonacichCentrality(firstCalc, append(vector))

	private def append(vector: BonacichVector): Map[BonacichVector, List[Map[Actor, Double]]] = {
		val previousVals: List[Map[Actor, Double]] = getNiceIterations(vector)
		val iterateOn: Map[Actor, Double] = getLastIter(vector)
		val nextIter: Map[Actor, Double] = new BonacichCentralityIter(iterateOn, vector).next.centralityValues
		iterations.updated(vector, nextIter :: previousVals)
	}

	private def getNiceIterations(vector: BonacichVector): List[Map[Actor, Double]] = {
		iterations.get(vector) match {
			case None => List()
			case Some(iters) => iters
		}
	}

	private def getLastIter(vector: BonacichVector): Map[Actor, Double] =
		iterations.get(vector) match {
			case None => firstCalc
			case Some(iters) => iters.head
		}

	class BonacichCentralityIter(val centralityValues: Map[Actor, Double], vector: BonacichVector) {

		def next(): BonacichCentralityIter =
			new BonacichCentralityIter((for (actor: (Actor, Double) <- centralityValues) yield calcNext(actor._1)), vector)

		private def calcNext(actor: Actor): (Actor, Double) = {
			(actor, calcNextDouble(actor))
		}

		private def calcNextDouble(actor: Actor): Double = {
			val nextDoubleNotRounded = (for (nextActor <- actor.getAllEndActors) yield vector.calc(centralityValues(nextActor))).sum
			BigDecimal(nextDoubleNotRounded).setScale(2, BigDecimal.RoundingMode.HALF_UP).doubleValue
		}
	}
}

//alpha is normalization vector, while beta is a number that determines the value of adjacent nodes.
//the bigger alpha is in relation to beta, the centrality of current node is more important in relation to other nodes.
//positive beta, nodes in the center are more important. negative beta, nodes on the edges of the graph are more important.
class BonacichVector(val alpha: Double, val beta: Double) {
	def calc(value: Double) = alpha + beta * value
}