package rs.fon.kvizic.networkAnalysis.algorithm.tarjan

import rs.fon.kvizic.networkAnalysis.stub.StubActor
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import rs.fon.kvizic.networkAnalysis.Network
import StubRepo._
import rs.fon.kvizic.networkAnalysis.Network
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TarjanSuite extends FunSuite {

  /**
   *              			stub1
   *              -->					-->
   *              stub2		<--			stub7
   *              -->					-->
   *   stub6 -->  stub3					stub8
   *   <--        -->
   *   stub5 <--  stub4
   *
   */
  test("test 1") {
    val network: Network = new Network(List(stub1, stub2, stub3, stub4, stub5, stub6, stub7, stub8))
    val components: List[Network] = new Tarjan(network).connectedComponents()
    assert(5 == components.size)
    val componentsSizeCount: Map[Int, List[Network]] = components.groupBy(network => network.actors.size)
    assert(1 == componentsSizeCount(4).size)
    assert(4 == componentsSizeCount(1).size)
  }

  /**
   * stub9 --> stub 10					stub16
   * 	   <-- -->
   * 		   stub11 --> stub 12       stub15
   * 			<--		  -->           -->
   * 			-		  stub13   -- 	stub14
   * 			---------------------------
   */

  test("test2") {
    val network: Network = new Network(List(stub9, stub10, stub11, stub12, stub13, stub14, stub15, stub16, stub17))
    val components: List[Network] = new Tarjan(network).connectedComponents()
    assert(4 == components.size)
    val componentsSizeCount: Map[Int, List[Network]] = components.groupBy(network => network.actors.size)
    assert(1 == componentsSizeCount(4).size)
    assert(1 == componentsSizeCount(3).size)
    assert(2 == componentsSizeCount(1).size)
  }

}

object StubRepo {

  val stub1: StubActor = new StubActor("1", List(stub2, stub7))
  val stub2: StubActor = new StubActor("2", List(stub3))
  val stub3: StubActor = new StubActor("3", List(stub4))
  val stub4: StubActor = new StubActor("4", List(stub5))
  val stub5: StubActor = new StubActor("5", List(stub6))
  val stub6: StubActor = new StubActor("6", List(stub3))
  val stub7: StubActor = new StubActor("7", List(stub2, stub8))
  val stub8: StubActor = new StubActor("8", List())

  val stub9: StubActor = new StubActor("9", List(stub10))
  val stub10: StubActor = new StubActor("10", List(stub11))
  val stub11: StubActor = new StubActor("11", List(stub9, stub12))
  val stub12: StubActor = new StubActor("12", List(stub13))
  val stub13: StubActor = new StubActor("13", List(stub14))
  val stub14: StubActor = new StubActor("14", List(stub15))
  val stub15: StubActor = new StubActor("15", List(stub14, stub16, stub12))
  val stub16: StubActor = new StubActor("16", List())
  val stub17: StubActor = new StubActor("17", List())

}