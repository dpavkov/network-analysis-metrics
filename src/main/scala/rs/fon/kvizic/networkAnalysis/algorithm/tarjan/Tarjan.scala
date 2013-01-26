package rs.fon.kvizic.networkAnalysis.algorithm.tarjan

import rs.fon.kvizic.networkAnalysis.Network

object Tarjan {
  def connectedComponents(network: Network): List[Network] = {
    val startingNodes: List[TarjanNode] = for (actor <- network.actors) yield new TarjanNode(actor)

    def connectedComponentsIter(root: TarjanNode, network: TarjanNetwork): List[TarjanNetwork] = {
      val dfsResult: TarjanNetwork = network.dfs(root, new TarjanNetwork(List(root)), new TarjanNetwork(List(root)))
      val iterComponents: List[TarjanNetwork] = (for (component <- dfsResult.nodes.groupBy(node => node.lowLink))
        yield new TarjanNetwork(component._2)) toList
      val networkLeftovers: List[TarjanNode] = network.nodes.filter(node => !(iterComponents.exists(component => component.contains(node.actor))))
      if (networkLeftovers.isEmpty) iterComponents
      else iterComponents ::: connectedComponentsIter(new TarjanNode(networkLeftovers.head.actor, 1, 1), new TarjanNetwork(networkLeftovers))
    }

    val components: List[TarjanNetwork] = connectedComponentsIter(new TarjanNode(startingNodes.head.actor, 1, 1), new TarjanNetwork(startingNodes))
    for (tarjanNetwork <- components) yield tarjanNetwork.toNetwork
  }
}