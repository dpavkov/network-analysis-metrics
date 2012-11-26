package rs.fon.kvizic.networkAnalysis
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar._

@RunWith(classOf[JUnitRunner])
class RelationSuite extends FunSuite {

  val mockEndActor = mock[Actor]

  test("test binary relation trait") {
    new BinaryRelation(mockEndActor) {
      def relType: RelationType = DefaultRelationType
      assert(1 == weight)
    }
  }

  test("test value relation trait") {
    val value = 0.64
    new ValuedRelation(value, mockEndActor) {
      def relType: RelationType = DefaultRelationType
      assert(value == weight)
    }
  }

  test("value out of boundaries must fail") {
    try {
      new ValuedRelation(1.64, mockEndActor) {
        def relType: RelationType = DefaultRelationType
      }.weight
      fail()
    } catch {
      case _: IllegalArgumentException =>
      case _ => fail()
    }
  }

  test("test best ranked relation") {
    val outDegree = 10;
    val rank = 0;
    assert(1.0 == (new StubRankRelation(outDegree, rank, mockEndActor)).weight)
  }

  test("test worst ranked weight") {
    val outDegree = 10;
    val rank = 9;
    val rankWeight = (new StubRankRelation(outDegree, rank, mockEndActor)).weight
    assert(0.1 == rankWeight, rankWeight + " is not the correct answer")
  }

  test("test default description") {
    assert(1.0 == (new DescriptiveRelation(DefaultDescriptionHigh, mockEndActor) {
      def relType: RelationType = DefaultRelationType
    }).weight)
  }

  test("test custom description") {
    val descVal = 0.2
    case object TestDescription extends Description("test", descVal)
    assert(descVal == (new DescriptiveRelation(TestDescription, mockEndActor) {
      def relType: RelationType = DefaultRelationType
    }).weight)
  }

  test("test custom description fail") {
    val descVal = -0.2
    case object TestDescriptionFail extends Description("test", descVal)
    try {
      val wei = (new DescriptiveRelation(TestDescriptionFail, mockEndActor) {
        def relType: RelationType = DefaultRelationType
      }).weight
      fail()
    } catch {
      case _: IllegalArgumentException =>
      case _ => fail()
    }
  }
}