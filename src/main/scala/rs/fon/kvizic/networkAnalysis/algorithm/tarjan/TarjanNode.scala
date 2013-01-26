package rs.fon.kvizic.networkAnalysis.algorithm.tarjan

import rs.fon.kvizic.networkAnalysis.Actor

class TarjanNode(val actor: Actor, val index: Int = 0, val lowLink: Int = 0) {

  override def toString = "actor: " + actor.toString + " index: " + index + " low link: " + lowLink

  val successors: List[Actor] = 
    (for (relType <- actor.allRelTypes)
    	yield actor.getEndActorsOrNone(relType) match {
    		case None => List()
    		case Some(endActors) => endActors
    }) flatten

  def minLowLink(otherLowLink: Int): TarjanNode = new TarjanNode(actor, index, lowLink.min(otherLowLink))

}
