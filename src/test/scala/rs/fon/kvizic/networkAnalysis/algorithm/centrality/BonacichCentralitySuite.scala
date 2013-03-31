package rs.fon.kvizic.networkAnalysis.algorithm.centrality

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import rs.fon.kvizic.networkAnalysis.Actor
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{ when, verify, times, reset }
import rs.fon.kvizic.networkAnalysis.RelationType
import scala.collection.immutable.List

@RunWith(classOf[JUnitRunner])
class BonacichCentralitySuite extends FunSuite {

	val actor1: Actor = mock[Actor]
	val actor2: Actor = mock[Actor]
	val actor3: Actor = mock[Actor]
	val actor4: Actor = mock[Actor]
	val actor5: Actor = mock[Actor]

	val firstCalc: Map[Actor, Double] = Map((actor1, 3.0), (actor2, 2.0), (actor3, 5.0), (actor4, 1.5), (actor5, 10.0));
	val vector: BonacichVector = new BonacichVector(1, 0.2)

	val centrality: BonacichCentrality = new BonacichCentrality(firstCalc, Map[BonacichVector, List[Map[Actor, Double]]]())

	private def mockGetAllEndActors() = {
		when(actor1.getAllEndActors).thenReturn(List(actor2, actor3))
		when(actor2.getAllEndActors).thenReturn(List(actor1, actor5))
		when(actor3.getAllEndActors).thenReturn(List(actor4))
		when(actor4.getAllEndActors).thenReturn(List(actor3))
		when(actor5.getAllEndActors).thenReturn(List(actor1, actor2, actor3, actor4))
	}

	private def simulateIter(vector: BonacichVector, centrality: BonacichCentrality, expextedResults: List[Double]): BonacichCentrality = {
		val nextCentrality = centrality.next(vector)
		val results: Map[Actor, Double] = nextCentrality.iterations(vector)(0)
		assert(expextedResults(0) == results(actor1), "expected " + expextedResults(0) + ", but was " + results(actor1))
		assert(expextedResults(1) == results(actor2))
		assert(expextedResults(2) == results(actor3))
		assert(expextedResults(3) == results(actor4))
		assert(expextedResults(4) == results(actor5))
		nextCentrality
	}

	private def verifyAndReset(called: Int) = {
		verify(actor1, times(called)).getAllEndActors
		verify(actor2, times(called)).getAllEndActors
		verify(actor3, times(called)).getAllEndActors
		verify(actor4, times(called)).getAllEndActors
		verify(actor5, times(called)).getAllEndActors

		reset(actor1, actor2, actor3, actor4, actor5)
	}

	test("Test first iter") {
		mockGetAllEndActors

		simulateIter(vector, centrality, List[Double](3.4, 4.6, 1.3, 2, 6.3))

		verifyAndReset(1)
	}

	test("Test second iter") {
		mockGetAllEndActors

		val nextCentrality = simulateIter(vector, centrality, List[Double](3.4, 4.6, 1.3, 2, 6.3))
		simulateIter(vector, nextCentrality, List[Double](3.18, 3.94, 1.4, 1.26, 6.26))

		verifyAndReset(2)
	}

}