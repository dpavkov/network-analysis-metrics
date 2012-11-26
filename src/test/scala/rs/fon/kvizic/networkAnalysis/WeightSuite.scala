package rs.fon.kvizic.networkAnalysis

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WeightSuite extends FunSuite {

  test("binary weight is always one") {
    assert(1 == (new BinaryWeight).weight)
  }

  test("valued return what`s given") {
    val value = 0.64
    assert(value == (new ValuedWeight(value)).weight)
  }

  test("value out of boundaries must fail") {
    try {
      (new ValuedWeight(1.64)).weight
      fail()
    } catch {
      case _: IllegalArgumentException =>
      case _ => fail()
    }
  }

  test("test best ranked weight") {
    val outDegree = 10;
    val rank = 0;
    assert(1.0 == (new RankWeight(outDegree, rank)).weight)
  }

  test("test worst ranked weight") {
    val outDegree = 10;
    val rank = 9;
    val rankWeight = (new RankWeight(outDegree, rank)).weight
    assert(0.1 == rankWeight, rankWeight + " is not the correct answer")
  }

  test("test default description") {
    assert(1.0 == (new DescriptiveWeight(DefaultDescriptionHigh)).weight)
  }

  test("test custom description") {
    val descVal = 0.2
    case object TestDescription extends Description("test", descVal)
    assert(descVal == (new DescriptiveWeight(TestDescription)).weight)
  }

  test("test custom description fail") {
    val descVal = -0.2
    case object TestDescriptionFail extends Description("test", descVal)
    try {
      val wei = (new DescriptiveWeight(TestDescriptionFail)).weight
      fail()
    } catch {
      case _: IllegalArgumentException =>
      case _ => fail()
    }
  }
}