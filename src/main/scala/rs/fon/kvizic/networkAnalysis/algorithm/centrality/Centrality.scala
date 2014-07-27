package rs.fon.kvizic.networkAnalysis.algorithm.centrality

import rs.fon.kvizic.networkAnalysis.Network
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.ShortestPath
import rs.fon.kvizic.networkAnalysis.Actor

class Centrality(shortestPath: ShortestPath) {
	
  lazy val shortestPaths = shortestPath.getPaths
  
  // iterates through actors of @param shortest paths, and for each calls getCentralityValueForActor in order to calculate closeness centrality
  def getCentralityValues():  Map[Actor, Double] = {
    ( for (actor <- shortestPath.getPaths.map(actorValuePair => actorValuePair._1)) 
    	yield (actor, getCentralityValueForActor(actor)) )
    	.toMap
  }
  
  private def getCentralityValueForActor(actor: Actor): Double = {
    // list of values of all shortest paths for @actor
    val weights: List[Double] = for (path <- shortestPaths(actor))
     yield path.getWeight
     // list of inverse shortest paths
    val weightsInverse = weights.map(wei => BigDecimal(1/wei).setScale(2, BigDecimal.RoundingMode.HALF_UP).doubleValue)
    // sums the inverse paths
    weightsInverse.sum
  }
}