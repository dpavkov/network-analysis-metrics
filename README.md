network-analysis-metrics


Network Analysis Metrics is an open source scala library that calculates SNA metrics for you application`s graph. 

Library description is provided on wiki pages, while here you can find usage examples

* Zoes tale example 

The domain describes two kinds of actors: Humans and Obin. Humans may like Humans, while Obin may like both Obin and Human. 

First, for each of the relations the object that extends RelationType is defined:
`case object LikeRelationType extends RelationType("like")
case object LikeObinRelationType extends RelationType("like obin")`

Next, for each of the relation types create a concrete class that defines what kind of relation it should be. This is done by extending one of the subclasses of Relation abstract class. For our example simple BinaryRelation is used which only says that the relation exists:

```
class Like(paramEndActor: Human) extends BinaryRelation(paramEndActor) {
  def updateEndActor(endActor: Actor): Like = endActor match{
    case human: Human => new Like(human)
    case _ => throw new IllegalArgumentException("you can like only a human")
  }
  def relType: RelationType = LikeRelationType
}

class LikeObin(paramEndActor: Obin) extends BinaryRelation(paramEndActor) {
  def updateEndActor(endActor: Actor): LikeObin = endActor match{
    case obin: Obin => new LikeObin(obin)
    case _ => throw new IllegalArgumentException("you can obinLike only obin!")
  }
  def relType: RelationType = LikeObinRelationType
}
```


The next step is to wire your domain objects to the metrics. This is done by implementing Actor trait.
```
class Obin(val name: String, val likes: List[Actor]) extends Actor {
override def toString: String = name

  override def equals(that: Any): Boolean = ???
  override def hashCode: Int = ???

def removeMode(relType: RelationType): Option[Actor] = 
  if (relType == LikeRelationType) None 
  else Some(this)

  def relations(): List[Mode] = {
    if (likes.isEmpty) List.empty
    else {
      val relations: List[Like] = (for (actor <- likes) yield {
        actor match {
          case stub: Human => new Like(stub)
          case _ => throw new IllegalArgumentException("relation not allowed")
        }
      }) toList
      val likeMode = new Mode(relations, LikeRelationType)
      likeMode :: Nil
    }

  }

  def updateRelations(newRels: List[Relation]): Actor = {
    val relType = newRels match {
      case List() => throw new IllegalArgumentException("Empty list doesn`t make sense")
      case head :: tail => head.relType
    }
    relType match {
      case LikeRelationType => updateLike(newRels)
      case unknownType => throw new IllegalArgumentException("Relation type " + unknownType.getClass() + " is not supported in Obin!")
    }
  }

  def updateLike(likes: List[Relation]): Obin = {
    val actors: List[Actor] = for (rel <- likes) yield rel match {
      case like: Like => like.endActor
      case _ => throw new IllegalArgumentException("unsupported relation")
    }
    new Obin(name, actors)
  }

}
```
```
class Human(val name: String, humans: => List[Human], obins: => List[Obin]) extends Actor {

  override def toString: String = name
  
  def removeMode(relType: RelationType): Option[Actor] = relType match {
    case LikeRelationType => 
    if (obins.isEmpty) None
      else Some(new Human(name, List(), obins))
    case LikeObinRelationType => 
      if (humans.isEmpty) None
      else Some(new Human(name, humans, List()))
    case _ => Some(this)
  }

  override def equals(that: Any): Boolean = ???
  override def hashCode: Int = ???

  def createLikesMode(humans: List[Human]): Option[Mode] = {
	humans match {
	  case List() => None
	  case humans: List[Human] => {
	    val likes: List[Relation] = for (human <- humans) yield new Like(human)
	    Option(new Mode(likes, LikeRelationType))
	  }
	}
  }
  
  def createLikesObinMode(obins: List[Obin]): Option[Mode] = {
	obins match {
	  case List() => None
	  case humans: List[Obin] => {
	    val likes: List[Relation] = for (obin <- obins) yield new LikeObin(obin)
	    Option(new Mode(likes, LikeObinRelationType))
	  }
	}
  } 
  
  def relations(): List[Mode] = {
    val likeHumanMode: Option[Mode] = createLikesMode(humans)
	val likeObinMode: Option[Mode] = createLikesObinMode(obins)
	(likeHumanMode, likeObinMode) match {
      case (Some(likeHumanMode), Some(likeObinMode)) => List(likeHumanMode, likeObinMode)
     case (Some(likeHumanMode), None) => List(likeHumanMode)
     case (None, Some(likeObinMode)) => List(likeObinMode)
     case (None, None) => List()
    }
  }

  def updateRelations(newRels: List[Relation]): Actor = {
    val relType = newRels match {
      case List() => throw new IllegalArgumentException("Empty list doesn`t make sense")
      case head :: tail => head.relType
    }
    relType match {
      case LikeRelationType => updateLike(newRels)
      case LikeObinRelationType => updateObinLike(newRels)
      case unknownType => throw new IllegalArgumentException("Relation type " + unknownType.getClass() + " is not supported in Human!")
    }
  }
  
  def updateObinLike(likes: List[Relation]): Human = {
    val newObinLikes: List[Obin] = 
      for (like <- likes) yield like.endActor match {
      case obin: Obin => obin
      case _ => throw new IllegalArgumentException("unsupported relation")
    }
    new Human(name, humans, newObinLikes)
  }

  def updateLike(likes: List[Relation]): Human = {
    val newLikes: List[Human] = 
      for (like <- likes) yield like.endActor match {
      case human: Human => human
      case _ => throw new IllegalArgumentException("unsupported relation")
    }
    new Human(name, newLikes, obins)
  }
}
```

Now, lets create a mock repository with several actors defined:
```
object DataRepository {

  val jane: Human = new Human("jane", List(john, zoe), List())
  val john: Human = new Human("john", List(jane, zoe), List())
  val zoe: Human = new Human("zoe", List(john, jane, gretchen, enzo), List(hickory, dickory))
  val gretchen: Human = new Human("gretchen", List(manfred), List())
  val manfred: Human = new Human("manfred", List(gretchen), List())
  val enzo: Human = new Human("enzo", List(zoe), List())
  val savitri: Human = new Human("savitri", List(john), List())
  val betsy: Human = new Human("betsy", List(savitri), List())
  val dickory: Obin = new Obin("dickory", List(zoe, john, jane))
  val hickory: Obin = new Obin("hickory", List(zoe, john, jane))

}
```

Finally, lets open the repl, create the network and calculate the metrics:
``` 
val zoesTale: Network = new Network(List(zoe, john, jane, dickory, enzo, gretchen, savitri, betsy, manfred, hickory))
                                                  //> zoesTale  : rs.fon.kvizic.networkAnalysis.Network = 
                                                  //| Network: List(
                                                  //| zoe: List(john, jane, gretchen, enzo, hickory, dickory), 
                                                  //| john: List(jane, zoe), 
                                                  //| jane: List(john, zoe), 
                                                  //| dickory: List(zoe, john, jane), 
                                                  //| enzo: List(zoe), 
                                                  //| gretchen: List(manfred), 
                                                  //| savitri: List(john), 
                                                  //| betsy: List(savitri), 
                                                  //| manfred: List(gretchen), 
                                                  //| hickory: List(zoe, john, jane))
  
  val likeIn: Map[Actor, Int] = zoesTale.inDegrees(LikeRelationType)
                                                  //> likeIn  : Map[rs.fon.kvizic.networkAnalysis.Actor,Int] = Map(gretchen -> 2, 
                                                  //| john -> 5, manfred -> 1, jane -> 4, zoe -> 5, enzo -> 1, savitri -> 1)
                                                  
  val likeOut: Map[Actor, Int] = zoesTale.outDegrees(LikeRelationType)
                                                  //> likeOut  : Map[rs.fon.kvizic.networkAnalysis.Actor,Int] = Map(gretchen -> 1,
                                                  //|  john -> 2, manfred -> 1, jane -> 2, zoe -> 4, enzo -> 1, savitri -> 1, hick
                                                  //| ory -> 3, betsy -> 1, dickory -> 3)
  
 	val likeObinOut: Map[Actor, Int] = zoesTale.outDegrees(LikeObinRelationType)
                                                  //> likeObinOut  : Map[rs.fon.kvizic.networkAnalysis.Actor,Int] = Map(zoe -> 2)
 	 
 	val likeObinIn: Map[Actor, Int] = zoesTale.inDegrees(LikeObinRelationType)
                                                  //> likeObinIn  : Map[rs.fon.kvizic.networkAnalysis.Actor,Int] = Map(hickory -> 
                                                  //| 1, dickory -> 1)
                                                  
   zoesTale.averageDegree(LikeRelationType)       //> res0: Double = 1.9
   zoesTale.averageDegree(LikeObinRelationType)   //> res1: Double = 2.0
 	
 	val components: List[Network] = zoesTale.stronglyConnectedComponents
                                                  //> components  : List[rs.fon.kvizic.networkAnalysis.Network] = List(
                                                  //| Network: List(
                                                  //| dickory: List(zoe, john, jane), 
                                                  //| hickory: List(zoe, john, jane), 
                                                  //| enzo: List(zoe), 
                                                  //| jane: List(john, zoe), 
                                                  //| john: List(jane, zoe), 
                                                  //| zoe: List(john, jane, gretchen, enzo, hickory, dickory)), 
                                                  //| Network: List(
                                                  //| manfred: List(gretchen), 
                                                  //| gretchen: List(manfred)), 
                                                  //| Network: List(
                                                  //| savitri: List(john)), 
                                                  //| Network: List(
                                                  //| betsy: List(savitri)))
 	
 	val likeComponents: List[Network] = zoesTale.stronglyConnectedComponents(LikeRelationType)
                                                  //> likeComponents  : List[rs.fon.kvizic.networkAnalysis.Network] = List(
                                                  //| Network: List(
                                                  //| manfred: List(gretchen), 
                                                  //| gretchen: List(manfred)), 
                                                  //| Network: List(
                                                  //| hickory: List(zoe, john, jane)), 
                                                  //| Network: List(
                                                  //| enzo: List(zoe), 
                                                  //| jane: List(john, zoe), 
                                                  //| john: List(jane, zoe), 
                                                  //| zoe: List(john, jane, gretchen, enzo)), 
                                                  //| Network: List(
                                                  //| betsy: List(savitri)), 
                                                  //| Network: List(
                                                  //| savitri: List(john)), 
                                                  //| Network: List(
                                                  //| dickory: List(zoe, john, jane)))
```

* Rank relation example

Implementation:

```
case object LikeRelationType extends RelationType("like")

class RankLike(endActor: Actor, rankParam: Int = 0, outDegreeParam: Int = 1) extends RankRelation(outDegreeParam, rankParam, endActor) {
  def updateEndActor(endActor: Actor) = new RankLike(endActor, rankParam, outDegreeParam)
  def relType: RelationType = LikeRelationType
  def updateWeight(outDegree: Int, rank: Int): Relation = new RankLike(endActor, rank, outDegree)
}

class Friend(friends: List[Friend], relations: List[RankLike] = List()) extends Actor {
  
  val rels: List[RankLike] = 
    if (relations.isEmpty) for 
    (friend <- friends) yield new RankLike(friend)
    else relations
    
  def relations(): List[Mode] =  List(new Mode(rels, LikeRelationType))

  def updateRelations(newRels: List[Relation]): Actor = { 
    newRels match {
      case likes: List[RankLike] =>  new Friend(friends, likes)
      case _ => this
    }
   
  }

  def removeMode(relType: RelationType): Option[Actor] = { throw new UnsupportedOperationException("only one mode") }

}
```
Runtime usage:

```
  val friend1: Friend = new Friend(List())        //> friend1  : rs.fon.kvizic.example.Friend = rs.fon.kvizic.example.Friend@191ce2
                                                  //| a
  val friend2: Friend = new Friend(List())        //> friend2  : rs.fon.kvizic.example.Friend = rs.fon.kvizic.example.Friend@16469
                                                  //| 0
  val friend3: Friend = new Friend(List())        //> friend3  : rs.fon.kvizic.example.Friend = rs.fon.kvizic.example.Friend@72cf6
                                                  //| 0
  val friend4: Friend = new Friend(List(friend1, friend2, friend3))
                                                  //> friend4  : rs.fon.kvizic.example.Friend = rs.fon.kvizic.example.Friend@1ac61
                                                  //| 03
  
  val synced: Friend = friend4.sync(LikeRelationType) match {
  case friend: Friend => friend
  }                                               //> synced  : rs.fon.kvizic.example.Friend = rs.fon.kvizic.example.Friend@ad8848
                                                  //| 
  
  for (rel <- synced.relations.head.relations) println(rel + " " + rel.weight)
                                                  //> RankRelation(3,0,rs.fon.kvizic.example.Friend@191ce2a) 1.0
                                                  //| RankRelation(3,1,rs.fon.kvizic.example.Friend@164690) 0.6666666666666666
                                                  //| RankRelation(3,2,rs.fon.kvizic.example.Friend@72cf60) 0.3333333333333333
```




