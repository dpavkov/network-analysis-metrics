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
}