package rs.fon.kvizic.networkAnalysis

import rs.fon.kvizic.networkAnalysis.algorithm.tarjan.Tarjan
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.ShortestPath
import rs.fon.kvizic.networkAnalysis.algorithm.centrality.Centrality

class DataHolder(network: Network) {

  private val tarjan = new Tarjan(network)
  private val shortestPath = new ShortestPath(network)
  private val centrality = new Centrality(shortestPath)
  
  lazy val connectedComponents = tarjan.connectedComponents
  lazy val centralityValues = centrality.getCentralityValues
}