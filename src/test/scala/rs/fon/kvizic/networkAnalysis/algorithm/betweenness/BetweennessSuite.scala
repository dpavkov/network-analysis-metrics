package rs.fon.kvizic.networkAnalysis.algorithm.betweenness

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{ when, verify, times, reset }
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.ShortestPath
import rs.fon.kvizic.networkAnalysis.Network
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.Path
import rs.fon.kvizic.networkAnalysis.Relation

@RunWith(classOf[JUnitRunner])
class BetweennessSuite extends FunSuite {

  val shortestPath: ShortestPath = mock[ShortestPath]
  val actor1: Actor = mock[Actor]
  val actor2: Actor = mock[Actor]
  val actor3: Actor = mock[Actor]
  val actor4: Actor = mock[Actor]
  val actor5: Actor = mock[Actor]
  val path1: Path = mock[Path]
  val path2: Path = mock[Path]
  val path3: Path = mock[Path]
  val path4: Path = mock[Path]
  val rel1: Relation = mock[Relation]
  val rel2: Relation = mock[Relation]
  val rel3: Relation = mock[Relation]
  val rel4: Relation = mock[Relation]

  test("test betweenness") {
     when(shortestPath.getPaths).thenReturn(Map[Actor, List[Path]]((actor1, List[Path](path1, path2, path3, path4))))
     when(path1.end).thenReturn(actor2)
     when(path2.end).thenReturn(actor3)
     when(path3.end).thenReturn(actor4)
     when(path4.end).thenReturn(actor5)
     when(path1.path).thenReturn(List[List[Relation]](List[Relation](rel1, rel2, rel3)))
     when(path2.path).thenReturn(List[List[Relation]](List[Relation](rel1)))
     when(path3.path).thenReturn(List[List[Relation]](List[Relation](rel1, rel2), List[Relation](rel3, rel2)))
     when(path4.path).thenReturn(List[List[Relation]](List[Relation](rel1, rel2, rel3, rel4)))
     when(rel1.endActor).thenReturn(actor3)
     when(rel2.endActor).thenReturn(actor4)
     when(rel3.endActor).thenReturn(actor2)
     when(rel4.endActor).thenReturn(actor5)
     val betweenness: Betweenness = new Betweenness(shortestPath)
     
     val betweennessValue = betweenness.getBetweennessValue
     assert(0.5 == betweennessValue(actor2))
     assert(0.75 == betweennessValue(actor3))
     assert(0.5 == betweennessValue(actor4))
     
     verify(path1, times(3)).end
     verify(path2).end
     verify(path3, times(4)).end
     verify(path4, times(4)).end
     verify(path1).path
     verify(path2).path
     verify(path3).path
     verify(path4).path
     verify(rel1, times(7)).endActor
     verify(rel2, times(6)).endActor
     verify(rel3, times(5)).endActor
     verify(rel4).endActor
     reset(path1, path2, path3, path4, rel1, rel2, rel3, rel4)
  }

}