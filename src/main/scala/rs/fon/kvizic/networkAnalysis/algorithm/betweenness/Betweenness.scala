package rs.fon.kvizic.networkAnalysis.algorithm.betweenness

import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.ShortestPath
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.Path

import scala.Double

class Betweenness(shortestPath: ShortestPath) {

	// retrieves betweenness values
	def getBetweennessValue(): Map[Actor, Double] = {
		shortestPath.getPaths.foldLeft(Map[Actor, Double]())(
		    (acc: Map[Actor, Double], paths: (Actor, List[Path])) => 
		      considerElements(acc, getMiddleElements(paths._2), paths._2.size))
	}

	def brokerage(): Map[Actor, Double] = {
		getBetweennessValue.map(((actorBetweennessTuple): (Actor, Double)) => (actorBetweennessTuple._1 ->
			{
			  val actorsSize = actorBetweennessTuple._1.getAllEndActors.size
			  if (actorsSize < 2) {
			 	  0
			  // if local clustering coeficient equals 0, then brokerage can't be calculated that way, so it
			  // must be some logical small value: twice less than smallest possible lcc when there is at least one
			  // connection among actor's neighbors: 1 / (k * (k - 1) * 2)
			  } else if (actorBetweennessTuple._1.localClusteringCoef == 0.0) {
				  val divider = 1 / (actorsSize * (actorsSize - 1) * 2)
				  actorBetweennessTuple._2 / divider
				}
				// else use local clustering coef
				else actorBetweennessTuple._2 / actorBetweennessTuple._1.localClusteringCoef
			}))
	}

	private def getMiddleElements(paths: List[Path]): Map[Actor, Double] = {
	  // retrieves elements in between other elements
		val middleElements: List[Actor] =
			for (
				path <- paths;
				singlePath <- path.path;
				rel <- singlePath if !(rel.endActor == path.end)
			) yield rel.endActor
		// and count occurences for each actor
		countOccurences(middleElements)
	}
	
	// counts the number of times that each of actor is shown in the list
	private def countOccurences(elements: List[Actor]): Map[Actor, Double] = {
		elements.foldLeft(Map[Actor, Double]())((occurs, elem) => addOrIncrease(occurs, elem, 1.0))
	}

	private def addOrIncrease(map: Map[Actor, Double], elem: Actor, addend: Double): Map[Actor, Double] = {
		map.get(elem) match {
			case None => map + (elem -> addend)
			case Some(prevVal: Double) => map.updated(elem, prevVal + addend)
		}
	}
	// increase @param acc (betweenness values map), for each @param middleElement, depending on @param pathsCount (number of shortest paths)
	private def considerElements(acc: Map[Actor, Double], middleElements: Map[Actor, Double], pathsCount: Int): Map[Actor, Double] = {
		middleElements.foldLeft(acc)((acc, actorOccurences) => 
		  
		  // actorOccurences - actor, number of occurences in the shortest paths
		  addOrIncrease(acc, actorOccurences._1, actorOccurences._2 / pathsCount))
	}
}