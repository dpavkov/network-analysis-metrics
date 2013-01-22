package rs.fon.kvizic.networkAnalysis.algorithm

import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.Network
import scala.collection.mutable.DoubleLinkedList

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

  class TarjanNode(val actor: Actor, val index: Int = 0, val lowLink: Int = 0) {

    override def toString = "actor: " + actor.toString + " index: " + index + " low link: " + lowLink

    val successors: List[Actor] = (for (relType <- actor.allRelTypes)
      yield actor.getEndActorsOrNone(relType) match {
      case None => List()
      case Some(endActors) => endActors
    }) flatten

    def minLowLink(otherLowLink: Int): TarjanNode = new TarjanNode(actor, index, lowLink.min(otherLowLink))

  }

  class TarjanNetwork(val nodes: List[TarjanNode]) {

    def reverseMinLowLink(lowLink: Int, changeLowLinkFor: Set[Int], visited: TarjanNetwork): TarjanNetwork = {
      val head = nodes.head
      if (head.lowLink <= lowLink) {
        val updatedRelations: List[TarjanNode] = for (node <- visited.nodes) yield
          if (changeLowLinkFor contains(node.lowLink)) 
          node.minLowLink(lowLink)
          else node
        new TarjanNetwork(updatedRelations)
        
      }
      else {
       new TarjanNetwork(nodes.tail).reverseMinLowLink(lowLink, changeLowLinkFor + head.lowLink, visited)
      }
    }

    override def toString = nodes.foldLeft("")((next, node) => node.toString + " " + next)

    def toNetwork: Network = new Network(for (node <- nodes) yield node.actor)

    def contains(actor: Actor) = nodes.exists(node => node.actor == actor)

    def find(actor: Actor): Option[TarjanNode] = {

      def findIter(nodesLeft: List[TarjanNode]): Option[TarjanNode] =
        if (nodesLeft.isEmpty) None
        else if (nodesLeft.head.actor == actor) Some(nodesLeft.head)
        else findIter(nodesLeft.tail)

      findIter(nodes)
    }

    def dfs(node: TarjanNode, stack: TarjanNetwork, visited: TarjanNetwork): TarjanNetwork = {
      def dfsIter(current: TarjanNode, successors: List[Actor], stack: TarjanNetwork, visited: TarjanNetwork): TarjanNetwork = {
        if (successors.isEmpty) visited
        else {
          this.find(successors.head) match {
            case None => dfsIter(current, successors.tail, stack, visited) // not in network
            case Some(nextActor) => {
              visited.find(nextActor.actor) match {
                case None => { // in network, not visited
                  val nextNodeIndex = visited.nodes.head.index + 1
                  val nextNode: TarjanNode = new TarjanNode(successors.head, nextNodeIndex, nextNodeIndex)
                  val nextStack = new TarjanNetwork(nextNode :: stack.nodes)
                  val nextVisited: TarjanNetwork = dfs(nextNode, nextStack, new TarjanNetwork(nextNode :: visited.nodes))
                  val nextCurrent: TarjanNode = current.minLowLink(nextVisited.nodes.head.lowLink)
                  val finalVisited: TarjanNetwork = nextVisited.replace(nextCurrent)
                  val finalStack = stack.replace(nextCurrent)
                  dfsIter(nextCurrent, successors.tail, finalStack, nextVisited)
                }
                case Some(nextNode) => { //in network, visited
                  stack.find(nextNode.actor) match {
                    case None => dfsIter(current, successors.tail, stack, visited) //in network, visited, not in stack
                    case Some(nextNode) => { //in network, visited, in stack
                      val nextVisited: TarjanNetwork = stack.reverseMinLowLink(nextNode.lowLink, Set(current.lowLink), visited)
                      val nextStack: TarjanNetwork = stack.replaceAll(nextVisited.nodes)
                      dfsIter(nextVisited.nodes.head, successors.tail, nextStack, nextVisited)
                    }
                  }

                }
              }
            }
          }
        }
      }

      dfsIter(node, node.successors, stack, visited)
    }

    def replace(node: TarjanNode): TarjanNetwork = new TarjanNetwork(nodes map (ofNode => if (ofNode.actor == node.actor) node else ofNode))

    def replaceAll(nodes: List[TarjanNode]): TarjanNetwork = nodes.foldLeft(this)((network, node) => network.replace(node))
  }
}