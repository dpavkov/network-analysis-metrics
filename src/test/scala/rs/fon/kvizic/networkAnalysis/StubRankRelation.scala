package rs.fon.kvizic.networkAnalysis

class StubRankRelation(paramOutDegree: Int, paramRank: Int, paramEndActor: Actor) extends RankRelation(paramOutDegree, paramRank, paramEndActor) {
  def relType: RelationType = DefaultRelationType
  def updateWeight(outDegree: Int, rank: Int): Relation = new StubRankRelation(outDegree, rank, endActor)
}
