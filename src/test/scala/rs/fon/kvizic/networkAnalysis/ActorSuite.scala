package rs.fon.kvizic.networkAnalysis

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{ when, verify, times, reset }

@RunWith(classOf[JUnitRunner])
class ActorSuite extends FunSuite {

  val mode1: Mode = mock[Mode]
  val mode2: Mode = mock[Mode]
  val rt1: RelationType = mock[RelationType]
  val rt2: RelationType = mock[RelationType]
  val rt3: RelationType = mock[RelationType]
  val rel1: Relation = mock[Relation]
  val rel2: Relation = mock[Relation]
  val rel3: Relation = mock[Relation]

  val actor: Actor = new Actor() {
    val modes: List[Mode] = List[Mode](mode1, mode2)
    def relations: List[Mode] = modes
    def updateRelations(newRels: List[Relation]): Actor = actor2
  }

  val actor2 = mock[Actor]
  val actor3 = mock[Actor]
  val actor4 = mock[Actor]

  test("test get mode") {
    when(mode1.relType).thenReturn(rt1)
    when(mode2.relType).thenReturn(rt2)
    assert(mode1 == actor.getModeOrNone(rt1).get)
    assert(mode2 == actor.getModeOrNone(rt2).get)
    assert(None == actor.getModeOrNone(rt3))
    verify(mode1, times(3)).relType
    verify(mode2, times(3)).relType
    reset(mode1, mode2)
  }

  test("test all rel types") {
    when(mode1.relType).thenReturn(rt1)
    when(mode2.relType).thenReturn(rt2)
    assert(2 == actor.allRelTypes.size)
    verify(mode1, times(1)).relType
    verify(mode2, times(1)).relType
    reset(mode1, mode2)
  }

  test("test out degree") {
    when(mode1.relType).thenReturn(rt1)
    when(mode2.relType).thenReturn(rt2)
    when(mode1.relations).thenReturn(List[Relation](rel1, rel2))
    when(mode2.relations).thenReturn(List[Relation](rel3))
    assert(2 == actor.outDegree(rt1))
    assert(1 == actor.outDegree(rt2))
    verify(mode1, times(2)).relType
    verify(mode2, times(2)).relType
    verify(mode1, times(1)).relations
    verify(mode2, times(1)).relations
    reset(mode1, mode2)
  }

  test("test get end actors") {
    when(mode1.relType).thenReturn(rt1)
    when(mode2.relType).thenReturn(rt3)
    when(mode1.relations).thenReturn(List[Relation](rel1, rel2))
    when(mode2.relations).thenReturn(List[Relation](rel3))
    when(rel1.endActor).thenReturn(actor2)
    when(rel2.endActor).thenReturn(actor3)
    assert(2 == actor.getEndActorsOrNone(rt1).get.size)
    assert(None == actor.getEndActorsOrNone(rt2))
    verify(mode1, times(2)).relType
    verify(mode2, times(2)).relType
    verify(mode1, times(1)).relations
    verify(rel1, times(1)).endActor
    verify(rel2, times(1)).endActor
    reset(mode1, mode2, rel1, rel2)
  }

  test("test get all end actors") {
    when(mode1.relType).thenReturn(rt1)
    when(mode2.relType).thenReturn(rt2)
    when(mode1.relations).thenReturn(List[Relation](rel1, rel2))
    when(mode2.relations).thenReturn(List[Relation](rel3))
    when(rel1.endActor).thenReturn(actor2)
    when(rel2.endActor).thenReturn(actor3)
    when(rel3.endActor).thenReturn(actor4)
    assert(3 == actor.getAllEndActors.size)
    verify(mode1, times(5)).relType
    verify(mode2, times(5)).relType
    verify(mode1, times(2)).relations
    verify(mode2, times(2)).relations
    verify(rel1, times(2)).endActor
    verify(rel2, times(2)).endActor
    verify(rel3, times(2)).endActor
    reset(mode1, mode2, rel1, rel2, rel3)
  }

  test("test sync") {
    when(mode1.relType).thenReturn(rt1)
    when(mode2.relType).thenReturn(rt2)
    when(mode1.relations).thenReturn(List[Relation](rel1, rel2))
    assert(actor2 == actor.sync(rt1))
    verify(mode1, times(1)).relType
    verify(mode2, times(1)).relType
    verify(mode1, times(1)).relations
    reset(mode1, mode2)
  }

  test("test add relation") {
    val mode3 = mock[Mode]
    when(rel3.relType).thenReturn(rt1)
    when(mode1.relType).thenReturn(rt1)
    when(mode2.relType).thenReturn(rt2)
    when(mode1.addOrReplaceRelation(rel3)).thenReturn(mode3)
    when(mode3.relations).thenReturn(List[Relation](rel3, rel2))
    assert(actor2 == actor.addRelation(rel3))
    verify(rel3, times(1)).relType
    verify(mode1, times(1)).relType
    verify(mode2, times(1)).relType
    verify(mode1, times(1)).addOrReplaceRelation(rel3)
    verify(mode3, times(1)).relations
    reset(rel3, mode1, mode2, mode3)
  }
}