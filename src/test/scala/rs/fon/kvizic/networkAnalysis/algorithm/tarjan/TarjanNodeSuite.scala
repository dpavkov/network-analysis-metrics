package rs.fon.kvizic.networkAnalysis.algorithm.tarjan

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{ when, verify, times, reset }
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.RelationType


@RunWith(classOf[JUnitRunner])
class TarjanNodeSuite extends FunSuite {
  
  test ("test no successors") {
    val actor = mock[Actor]
    val relType = mock[RelationType]
    
    when(actor.allRelTypes).thenReturn(List[RelationType](relType))
    when(actor.getEndActorsOrNone(relType)).thenReturn(None)
    
    val node = new TarjanNode(actor)
    assert(node.successors.isEmpty)
    
    verify(actor, times(1)).allRelTypes
    verify(actor, times(1)).getEndActorsOrNone(relType)
    reset(actor)
  }

}