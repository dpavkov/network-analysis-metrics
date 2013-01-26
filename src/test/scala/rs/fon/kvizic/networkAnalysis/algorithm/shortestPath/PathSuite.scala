package rs.fon.kvizic.networkAnalysis.algorithm.shortestPath

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{ when, verify, times, reset }
import rs.fon.kvizic.networkAnalysis.Relation
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.Path
import sun.security.tools.policytool.NewPolicyPermOKButtonListener

@RunWith(classOf[JUnitRunner])
class PathSuite extends FunSuite{
  
    def setUp: (Actor, Relation, Relation, Relation) = (mock[Actor], mock[Relation], mock[Relation], mock[Relation])
  
  test("test get weight in shortest path") {
    val (start, rel1, rel2, rel3) = setUp
    val path: Path = new Path(start, List[List[Relation]](List[Relation](rel1, rel2, rel3)))
    
    when(rel1.weight).thenReturn(0.7)
    when(rel2.weight).thenReturn(0.6)
    when(rel3.weight).thenReturn(0.9)
    
    assert(2.2 == path.getWeight)
    
    verify(rel1, times(1)).weight
    verify(rel2, times(1)).weight
    verify(rel3, times(1)).weight
    reset(rel1, rel2, rel3)
  }
    
    test("test consider path better") {
       val (start, rel1, rel2, rel3) = setUp
       val (end, rel4, rel5, rel6) = setUp
       val me: Path = new Path(start, List[List[Relation]](List[Relation](rel1, rel2, rel3)))
       val contendingPath: Path = new Path(start, List[List[Relation]](List[Relation](rel4, rel5, rel6)))
       when(rel1.endActor).thenReturn(end)
       when(rel4.endActor).thenReturn(end)
       when(rel1.weight).thenReturn(1)
       when(rel2.weight).thenReturn(0.6)
       when(rel3.weight).thenReturn(0.8)
       when(rel4.weight).thenReturn(1)
       when(rel5.weight).thenReturn(1)
       when(rel6.weight).thenReturn(0.3)
       
       val newPath: Path = me.considerPath(contendingPath)
       assert(2.3 == newPath.getWeight)
       assert(contendingPath == newPath)
       assert(start == newPath.start)
       assert(end == newPath.end)
       
       verify(rel1, times(1)).endActor
       verify(rel4, times(1)).endActor 
       verify(rel1, times(1)).weight
       verify(rel2, times(1)).weight
       verify(rel3, times(1)).weight
       verify(rel4, times(1)).weight
       verify(rel5, times(1)).weight
       verify(rel6, times(1)).weight
       reset(rel1, rel2, rel3, rel4, rel5, rel6)
    }
    
    test("test consider path equal") {
       val (start, rel1, rel2, rel3) = setUp
       val (end, rel4, rel5, rel6) = setUp
       val me: Path = new Path(start, List[List[Relation]](List[Relation](rel1, rel2, rel3)))
       val contendingPath: Path = new Path(start, List[List[Relation]](List[Relation](rel4, rel5, rel6)))
       when(rel1.endActor).thenReturn(end)
       when(rel4.endActor).thenReturn(end)
       when(rel1.weight).thenReturn(1)
       when(rel2.weight).thenReturn(0.6)
       when(rel3.weight).thenReturn(0.8)
       when(rel4.weight).thenReturn(1)
       when(rel5.weight).thenReturn(1)
       when(rel6.weight).thenReturn(0.4)
       
       val newPath: Path = me.considerPath(contendingPath)
       assert(2.4 == newPath.getWeight)
       assert(2 == newPath.path.size)
       
       verify(rel1, times(1)).endActor
       verify(rel4, times(1)).endActor 
       verify(rel1, times(1)).weight
       verify(rel2, times(1)).weight
       verify(rel3, times(1)).weight
       verify(rel4, times(2)).weight
       verify(rel5, times(2)).weight
       verify(rel6, times(2)).weight
       reset(rel1, rel2, rel3, rel4, rel5, rel6)
    }
    
    test("test consider path worse") {
       val (start, rel1, rel2, rel3) = setUp
       val (end, rel4, rel5, rel6) = setUp
       val me: Path = new Path(start, List[List[Relation]](List[Relation](rel1, rel2, rel3)))
       val contendingPath: Path = new Path(start, List[List[Relation]](List[Relation](rel4, rel5, rel6)))
       when(rel1.endActor).thenReturn(end)
       when(rel4.endActor).thenReturn(end)
       when(rel1.weight).thenReturn(1)
       when(rel2.weight).thenReturn(0.6)
       when(rel3.weight).thenReturn(0.8)
       when(rel4.weight).thenReturn(1)
       when(rel5.weight).thenReturn(1)
       when(rel6.weight).thenReturn(0.5)
       
       val newPath: Path = me.considerPath(contendingPath)
       assert(2.4 == newPath.getWeight)
       assert(1 == newPath.path.size)
       
       verify(rel1, times(1)).endActor
       verify(rel4, times(1)).endActor 
       verify(rel1, times(1)).weight
       verify(rel2, times(1)).weight
       verify(rel3, times(1)).weight
       verify(rel4, times(1)).weight
       verify(rel5, times(1)).weight
       verify(rel6, times(1)).weight
       reset(rel1, rel2, rel3, rel4, rel5, rel6)
    }

}