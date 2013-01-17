package rs.fon.kvizic.networkAnalysis

abstract class Weight {
  def weight: Double = {
    require(value > 0 && value <= 1, value + " is not valid. Value must not be 0 or less and it can not be higher than one!")
    value
  }

  protected def value: Double
}

case class BinaryWeight extends Weight {
  def value: Double = 1
}

case class ValuedWeight(v: Double) extends Weight {
  def value: Double = v
}

//the best ranked weight is the first in the 0-based collection of relations of the same type. rank is index in the collection, out degree size of the collection. the best ranked relation has weight 1, the worst ranked 1/out degree
case class RankWeight(outDegree: Int, rank: Int) extends Weight {
  def value: Double = (outDegree - rank).toDouble / outDegree
}

case class DescriptiveWeight[+D <: Description](desc: D) extends Weight {
  def value: Double = desc.value
}

