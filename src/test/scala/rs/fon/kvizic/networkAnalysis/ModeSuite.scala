package rs.fon.kvizic.networkAnalysis

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.scalatest.mock.MockitoSugar._

import org.mockito.Mockito.{ when, verify, times, reset }

@RunWith(classOf[JUnitRunner])
class ModeSuite extends FunSuite {

  val r1: Relation = mock[Relation]
  val r2: Relation = mock[Relation]
  val rt = mock[RelationType]
  val rt2 = mock[RelationType]
  val mode: Mode = new Mode(List(r1, r2), rt)

  test("test mode setup") {
    assert(2 == mode.relations.size)
  }

  test("test add") {
    val r3: Relation = mock[Relation]
    when(r3.relType).thenReturn(rt)
    val newMode: Mode = mode.addRelation(r3)
    assert(r3 == newMode.relations.head)
    verify(r3, times(1)).relType
    assert(3 == newMode.relations.size)
    reset(r3)
  }

  test("test add throws iae") {
    val r3: BinaryRelation = mock[BinaryRelation]
    when(r3.relType).thenReturn(rt2)
    try {
      mode.addRelation(r3)
      fail("Test failed, exception not thrown")
    } catch {
      case _: IllegalArgumentException =>
      case e => fail("wrong kind of exception thrown: " + e.getClass())
    }
    verify(r3, times(1)).relType
    reset(r3)
  }

  test("test add or replace rel") {
    val mockActor1: Actor = mock[Actor]
    val mockActor2: Actor = mock[Actor]
    val mockActor3: Actor = mock[Actor]
    val r3: Relation = mock[Relation]
    val r4: Relation = mock[Relation]
    when(r3.relType).thenReturn(rt)
    when(r4.relType).thenReturn(rt)
    when(r1.endActor).thenReturn(mockActor1)
    when(r2.endActor).thenReturn(mockActor2)
    when(r3.endActor).thenReturn(mockActor2)
    when(r4.endActor).thenReturn(mockActor3)
    val mode2: Mode = mode.addOrReplaceRelation(r3)
    val mode3: Mode = mode.addOrReplaceRelation(r4)
    assert(2 == mode2.relations.size)
    assert(3 == mode3.relations.size)
    verify(r3, times(1)).relType
    verify(r4, times(1)).relType
    verify(r1, times(3)).endActor
    verify(r2, times(3)).endActor
    verify(r3, times(2)).endActor
    verify(r4, times(1)).endActor
    reset(r1, r2, r3, r4)
  }

  test("test add all") {
    val r3: Relation = mock[Relation]
    val r4: Relation = mock[Relation]
    when(r3.relType).thenReturn(rt)
    when(r4.relType).thenReturn(rt)
    val newMode: Mode = mode.addAllRelations(List[Relation](r3, r4))
    assert(r3 == newMode.relations.head)
    assert(r4 == newMode.relations.tail.head)
    assert(4 == newMode.relations.size)
    verify(r3, times(1)).relType
    verify(r4, times(1)).relType
    reset(r3, r4)
  }

  test("test add all wrong type") {
    val r3: Relation = mock[Relation]
    val r4: Relation = mock[Relation]
    when(r3.relType).thenReturn(rt)
    when(r4.relType).thenReturn(rt2)
    val newMode: Mode = mode.addAllRelations(List[Relation](r3, r4))
    assert(r3 == newMode.relations.head)
    assert(r1 == newMode.relations.tail.head)
    assert(3 == newMode.relations.size)
    verify(r3, times(1)).relType
    verify(r4, times(1)).relType
    reset(r3, r4)
  }

  test("test remove") {
    val newMode: Mode = mode.removeRelations(List[Relation](r2))
    assert(1 == newMode.relations.size)
  }

  test("test connected") {
    val mockActor1: Actor = mock[Actor]
    val mockActor2: Actor = mock[Actor]
    val mockActor3: Actor = mock[Actor]
    when(r1.endActor).thenReturn(mockActor1)
    when(r2.endActor).thenReturn(mockActor2)
    assert(mode.connected(mockActor1))
    assert(mode.connected(mockActor2))
    assert(!mode.connected(mockActor3))
    verify(r1, times(3)).endActor
    verify(r2, times(2)).endActor
    reset(r1, r2)
  }
}