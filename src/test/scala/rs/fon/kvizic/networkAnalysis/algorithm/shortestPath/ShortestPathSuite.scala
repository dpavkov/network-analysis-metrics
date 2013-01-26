package rs.fon.kvizic.networkAnalysis.algorithm.shortestPath

import rs.fon.kvizic.networkAnalysis.stub.StubActor
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import StubRepo._
import org.scalatest.FunSuite
import rs.fon.kvizic.networkAnalysis.Network
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.stub.StubActorValue


@RunWith(classOf[JUnitRunner])
class ShortestPathSuite extends FunSuite {
  
    /**
   *              			stub1
   *              -->					-->
   *              stub2					stub3
   *              		-->			  --
   *   						stub4	
   */
  test("test 1") {
    val network: Network = new Network(List(stub1, stub2, stub3, stub4))
    
  	val shortestPathsForActors: Map[Actor, List[Path]] = new ShortestPath(network).getPaths()
  	val shortestPath1 = shortestPathsForActors(stub1)
  	val shortestPath2 = shortestPathsForActors(stub2)
  	val shortestPath3 = shortestPathsForActors(stub3)
  	val shortestPath4 = shortestPathsForActors(stub4)
  	
  	assert(3 == shortestPath1.size)
  	assert(2 == shortestPath2.size)
  	assert(1 == shortestPath3.size)
  	assert(1 == shortestPath4.size)
  }
  
   /**
   *              			stub1
   *              --> 0.2			--> 0.6
   *              stub2		--> 0.5			stub3
   *              
   */
  test("test 2") {
    val network: Network = new Network(List(stub5, stub6, stub7))
    
  	val shortestPathsForActors: Map[Actor, List[Path]] = new ShortestPath(network).getPaths()
  	val shortestPath1 = shortestPathsForActors(stub5)
  	val shortestPath2 = shortestPathsForActors(stub6)
  	val shortestPath3 = shortestPathsForActors(stub7)
  	
  	assert(2 == shortestPath1.size)
  	assert(1 == shortestPath2.size)
  	assert(0 == shortestPath3.size)
  	
  	assert(0.2 == shortestPath1.tail.head.getWeight, "0.2 not equal to " + shortestPath1.tail.head.getWeight)
  	assert(0.6 == shortestPath1.head.getWeight, "0.6 not equal to " + shortestPath1.head.getWeight)
  }
  
  /**		
   * 					stub8
   * 		--> 0.3				--> 0.2
   * 	stub9						stub10			stub15
   * 	--> 0.8										--> 0.8
   * 	stub11										stub16
   * 	--> 0.5	   0.5 -- 1
   * 	stub12       --> 0.5			stub13
   * 	0.4 <--
   * 	stub14
   * 
   */
  
  test("test 3") {
    val network: Network = new Network(List(stub8, stub9, stub10, stub11, stub12, stub13, stub14, stub15, stub16))
    
  	val shortestPathsForActors: Map[Actor, List[Path]] = new ShortestPath(network).getPaths()
  	val shortestPath8 = shortestPathsForActors(stub8)
  	val shortestPath9 = shortestPathsForActors(stub9)
  	val shortestPath10 = shortestPathsForActors(stub10)
  	val shortestPath11 = shortestPathsForActors(stub11)
  	val shortestPath12 = shortestPathsForActors(stub12)
  	val shortestPath13 = shortestPathsForActors(stub13)
  	val shortestPath14 = shortestPathsForActors(stub14)
  	val shortestPath15 = shortestPathsForActors(stub15)
  	val shortestPath16 = shortestPathsForActors(stub16)
  	
  	assert (2 == shortestPath11.head.path.size)
  	assert (1 == shortestPath11.head.getWeight)
  	
  	assert (5 == shortestPath8.size)
  	
  	assert (3 == shortestPath14.size)
  	
  	assert (1 == shortestPath15.size)
  	assert (0 == shortestPath16.size)
  }

}



object StubRepo {

  val stub1: StubActor = new StubActor("1", List(stub2, stub3))
  val stub2: StubActor = new StubActor("2", List(stub4))
  val stub3: StubActor = new StubActor("3", List(stub4))
  val stub4: StubActor = new StubActor("4", List(stub3))
  
  val stub5: StubActorValue = new StubActorValue("5", Map((stub6 -> 0.2), (stub7 -> 0.6)))
  val stub6: StubActorValue = new StubActorValue("6", Map((stub7 -> 0.5)))
  val stub7: StubActorValue = new StubActorValue("7", Map())
  
  val stub8: StubActorValue = new StubActorValue("8",  Map((stub9 -> 0.3), (stub10 -> 0.2)))
  val stub9: StubActorValue = new StubActorValue("9", Map((stub11 -> 0.8)))
  val stub10: StubActorValue = new StubActorValue("10", Map())
  val stub11: StubActorValue = new StubActorValue("11", Map((stub12 -> 0.5), (stub13 -> 1)))
  val stub12: StubActorValue = new StubActorValue("12", Map((stub13 -> 0.5)))
  val stub13: StubActorValue = new StubActorValue("13", Map((stub11 -> 0.5)))
  val stub14: StubActorValue = new StubActorValue("14", Map((stub12 -> 0.4)))
  val stub15: StubActorValue = new StubActorValue("15", Map((stub16 -> 0.8)))
  val stub16: StubActorValue = new StubActorValue("16", Map())
}
