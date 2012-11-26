package rs.fon.kvizic.networkAnalysis

abstract class Relation(val endActor: Actor) {

  def weight: Double = {
    require(value > 0 && value <= 1, value + " is not valid. Value must not be 0 or less and it can not be higher than one!")
    value
  }
  def value: Double

  def relType: RelationType
}

abstract case class BinaryRelation(paramEndActor: Actor) extends Relation(paramEndActor) {
  def value: Double = 1
}

abstract case class ValuedRelation(paramWeight: Double, paramEndActor: Actor) extends Relation(paramEndActor) {
  def value: Double = paramWeight
}

abstract case class RankRelation(outDegree: Int, rank: Int, paramEndActor: Actor) extends Relation(paramEndActor) {
  def value: Double = (outDegree - rank).toDouble / outDegree
  def updateWeight(outDegree: Int, rank: Int): Relation
}

//extend description with case objects to for eligable values in descriptive weight. For example, see default descriptions below 
abstract class Description(val descr: String, val value: Double)
case object DefaultDescriptionHigh extends Description("high", 1)
case object DefaultDescriptionNeutral extends Description("neutral", 0.5)
case object DefaultDescriptionLow extends Description("low", 0.1)

abstract case class DescriptiveRelation[D <: Description](desc: D, paramEndActor: Actor) extends Relation(paramEndActor) {
  def value = desc.value
}

abstract class RelationType(val relType: String)
case object DefaultRelationType extends RelationType("relates")
