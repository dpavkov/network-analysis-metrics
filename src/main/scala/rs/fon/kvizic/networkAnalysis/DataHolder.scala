package rs.fon.kvizic.networkAnalysis

import rs.fon.kvizic.networkAnalysis.algorithm.tarjan.Tarjan
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.ShortestPath
import rs.fon.kvizic.networkAnalysis.algorithm.centrality.Centrality
import rs.fon.kvizic.networkAnalysis.algorithm.betweenness.Betweenness
import rs.fon.kvizic.networkAnalysis.algorithm.centrality.BonacichCentrality
import rs.fon.kvizic.networkAnalysis.algorithm.centrality.BonacichVector

class DataHolder(network: Network) {

	private val tarjan = new Tarjan(network)
	private val shortestPath = new ShortestPath(network)
	private val centrality = new Centrality(shortestPath)
	private val betweenness = new Betweenness(shortestPath)
	var bonacich = new BonacichCentrality(centralityValues, Map[BonacichVector, List[Map[Actor, Double]]]())

	lazy val connectedComponents = tarjan.connectedComponents
	lazy val centralityValues = centrality.getCentralityValues
	lazy val betweennessValues = betweenness.getBetweennessValue
	lazy val brokerage = betweenness.brokerage

}