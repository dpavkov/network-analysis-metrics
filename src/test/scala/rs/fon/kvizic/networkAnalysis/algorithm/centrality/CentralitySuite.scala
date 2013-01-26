package rs.fon.kvizic.networkAnalysis.algorithm.centrality

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{ when, verify, times, reset }
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.ShortestPath
import rs.fon.kvizic.networkAnalysis.Network
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.Path

@RunWith(classOf[JUnitRunner])
class CentralitySuite extends FunSuite {
  
  val shortestPath: ShortestPath = mock[ShortestPath]
  val actor1: Actor = mock[Actor]
  val path1 : Path = mock[Path]
  val path2 : Path = mock[Path]
  val path3 : Path = mock[Path]

  val centrality: Centrality = new Centrality(shortestPath)
  
  test("test centrality") {
    when(shortestPath.getPaths).thenReturn(Map[Actor, List[Path]]((actor1, List[Path](path1, path2, path3))))
    when(path1.getWeight).thenReturn(1)
    when(path2.getWeight).thenReturn(2)
    when(path3.getWeight).thenReturn(0.5)
    
    assert(3.5 == centrality.getCentralityValues()(actor1))
    
    verify(shortestPath, times(2)).getPaths
    verify(path1).getWeight
    verify(path2).getWeight
    verify(path3).getWeight
    reset(shortestPath, path1, path2, path3)
  }
}