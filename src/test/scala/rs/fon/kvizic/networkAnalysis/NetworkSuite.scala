package rs.fon.kvizic.networkAnalysis

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{ when, verify, times, reset }

@RunWith(classOf[JUnitRunner])
class NetworkSuite extends FunSuite {

  val actor1: Actor = mock[Actor]
  val actor2: Actor = mock[Actor]
  val actor3: Actor = mock[Actor]
  val actor4: Actor = mock[Actor]
  val network: Network = new Network(List[Actor](actor1, actor2, actor3))
  val mode1: Mode = mock[Mode]
  val mode2: Mode = mock[Mode]
  val relation1: Relation = mock[Relation]
  val relation2: Relation = mock[Relation]
  val relation3: Relation = mock[Relation]

  val rt: RelationType = mock[RelationType]

  test("test remove") {
    assert(2 == network.removeActor(actor3).get.actors.size)
    assert(3 == network.removeActor(actor4).get.actors.size)
  }

  test("test remove all") {
    assert(2 == network.removeActors(List(actor3)).get.actors.size)
    assert(2 == network.removeActors(List(actor3, actor4)).get.actors.size)
    assert(1 == network.removeActors(List(actor1, actor3, actor4)).get.actors.size)
    assert(None == network.removeActors(List(actor1, actor3, actor2)))
  }

  test("test add or replace actor") {
    assert(3 == network.addOrReplaceActor(actor3).actors.size)
    assert(4 == network.addOrReplaceActor(actor4).actors.size)
  }

  test("test add or replace all") {
    assert(3 == network.addOrReplaceActors(List(actor3)).actors.size)
    assert(4 == network.addOrReplaceActors(List(actor3, actor4)).actors.size)
  }

  test("test out degrees") {
    when(actor1.outDegree(rt)).thenReturn(1)
    when(actor2.outDegree(rt)).thenReturn(3)
    when(actor3.outDegree(rt)).thenReturn(6)
    assert(3 == network.outDegrees(rt).size)
    verify(actor1, times(2)).outDegree(rt)
    verify(actor2, times(2)).outDegree(rt)
    verify(actor3, times(2)).outDegree(rt)
    reset(actor1, actor2, actor3)
  }

  test("test in degrees") {
    when(actor1.getEndActorsOrNone(rt)).thenReturn(Some(List(actor2)))
    when(actor2.getEndActorsOrNone(rt)).thenReturn(Some(List(actor1, actor3)))
    when(actor3.getEndActorsOrNone(rt)).thenReturn(None)
    assert(3 == network.inDegrees(rt).size)
    verify(actor1, times(1)).getEndActorsOrNone(rt)
    verify(actor2, times(1)).getEndActorsOrNone(rt)
    verify(actor3, times(1)).getEndActorsOrNone(rt)
    reset(actor1, actor2, actor3)
  }
  
  test("test average degree") {
    when(actor1.outDegree(rt)).thenReturn(0)
    when(actor2.outDegree(rt)).thenReturn(3)
    when(actor3.outDegree(rt)).thenReturn(6)
    assert(3 == network.averageDegree(rt))
    verify(actor1).outDegree(rt)
    verify(actor2, times(2)).outDegree(rt)
    verify(actor3, times(2)).outDegree(rt) 
    reset(actor1, actor2, actor3)
    
  }
  
  test("test filter by rel type") {
    when(actor1.filterByRelType(rt)).thenReturn(Some(actor1))
    when(actor2.filterByRelType(rt)).thenReturn(Some(actor2))
    when(actor3.filterByRelType(rt)).thenReturn(None)
    when(actor1.getModeOrNone(rt)).thenReturn(Some(mode1))
    when(actor2.getModeOrNone(rt)).thenReturn(Some(mode2))
    when(mode1.relations).thenReturn(List(relation1))
    when(mode2.relations).thenReturn(List(relation2))
    when(relation1.endActor).thenReturn(actor2)
    when(relation1.updateEndActor(actor2)).thenReturn(relation3)
    when(relation2.endActor).thenReturn(actor3)
    when(actor1.updateRelations(List(relation3))).thenReturn(actor2)
    when(actor2.updateRelations(List(relation2))).thenReturn(actor4)
    val filtered: Network = network.filterByRelType(rt).get
    assert(2 == filtered.actors.size)
    assert(actor4 == filtered.actors.head)
    assert(actor2 == filtered.actors.tail.head)
    verify(actor1).filterByRelType(rt)
    verify(actor2).filterByRelType(rt)
    verify(actor3).filterByRelType(rt)
    verify(actor1).getModeOrNone(rt)
    verify(actor2).getModeOrNone(rt)
    verify(mode1).relations
    verify(mode2).relations
    verify(relation1).endActor
    verify(relation2, times(2)).endActor
    verify(relation1).updateEndActor(actor2)
    verify(actor1).updateRelations(List(relation3))
    verify(actor2).updateRelations(List(relation2))
    reset(actor1, actor2, actor3, mode1, mode2, relation1, relation2)
  }
}