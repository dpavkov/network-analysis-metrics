package rs.fon.kvizic.networkAnalysis

class Mode(val relations: List[Relation], val relType: RelationType) {

  require(!relations.isEmpty, "relations list is of no use empty!")

  def addRelation(rel: Relation): Mode = {
    val newRelType: RelationType = rel.relType
    if (newRelType == relType)
      new Mode(rel :: relations, relType)
    else throw new IllegalArgumentException("relation type must be of type " + relType.relType + ", but was " + newRelType.relType)
  }

  //warning: depands on add relation to do relation type check!
  def addOrReplaceRelation(rel: Relation): Mode = {
    if (!connected(rel.endActor))
      addRelation(rel)
    else {
      val modeWithExRelRemoved: Mode = removeRelation(rel.endActor)
      modeWithExRelRemoved.addRelation(rel)
    }
  }

  //doesn`t throw iae as addRelation, just doesn`t add inappropriate vals
  def addAllRelations(rels: List[Relation]): Mode = {
    val correctRels: List[Relation] = rels.filter(r => r.relType == relType)
    new Mode(correctRels ::: relations, relType)
  }

  def removeRelation(actor: Actor): Mode = new Mode(relations.filter(r => r.endActor == actor), relType)

  def removeRelations(rels: List[Relation]): Mode = new Mode(relations.filter(r => !rels.contains(r)), relType)

  def connected(actor: Actor): Boolean = relations.exists(r => actor == r.endActor)

}