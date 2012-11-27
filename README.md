network-analysis-metrics


Network Analysis Metrics is an open source scala library that calculates SNA metrics for you application`s graph. 

Currently the following metrics are implemented:

  *  in degree.
  *  out degree
  *  strongly connected components

Detailed explanation of each of the metrics is provided in wiki pages

More metrics to come.

* Example usage *

Example project will be setup in the near future. Until then, please read the following text on how to use this library:

The domain describes two kinds of actors: Humans and Obin. Humans may like Humans, while Obin may like both Obin and Human. 

First, for each of the relations the object that extends RelationType is defined:

```
case object LikeRelationType extends RelationType("like")
case object LikeObinRelationType extends RelationType("like obin")
```

Next, for each of the relation types create a concrete class that defines what kind of relation it should be. This is done by extending one of the subclasses of Relation abstract class. For our example simple BinaryRelation is used which only says that the relation exists:

```
class Like(paramEndActor: Human) extends BinaryRelation(paramEndActor) {
  def relType: RelationType = LikeRelationType
}

class LikeObin(paramEndActor: Obin) extends BinaryRelation(paramEndActor) {
  def relType: RelationType = LikeObinRelationType
}
```

The next step is to wire your domain objects to the metrics. This is done by implementing Actor trait. It has two abstract methods. The first one, relations, which returns a list of modes. Mode is entity used internally by the library and it holds all the relations of one type for the actor. The second one is  updateRelations(newRels: List[Relation]). It returns the actor with all the relations of the current actor of the type of input list replaced by input argument. Here`s how it looks like in our case:

```
class Obin(val name: String, val likes: List[Actor]) extends Actor {
override def toString: String = name

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

  override def equals(that: Any): Boolean = that match {
    case human: Human => name == human.name
    case _ => false
  }

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
val zoesTale: Network = new Network(List(zoe, john, jane, hickory, dickory, enzo, gretchen, savitri, betsy, manfred))
                                                  //> zoesTale  : rs.fon.kvizic.networkAnalysis.Network = rs.fon.kvizic.networkAna
                                                  //| lysis.Network@ed09f6
  
  val likeIn: Map[Actor, Int] = zoesTale.inDegrees(LikeRelationType)
                                                  //> likeIn  : Map[rs.fon.kvizic.networkAnalysis.Actor,Int] = Map(john -> 5, jane
                                                  //|  -> 4, gretchen -> 2, zoe -> 5, manfred -> 1, savitri -> 1, enzo -> 1)
                                                  
  val likeOut: Map[Actor, Int] = zoesTale.outDegrees(LikeRelationType)
                                                  //> likeOut  : Map[rs.fon.kvizic.networkAnalysis.Actor,Int] = Map(betsy -> 1, jo
                                                  //| hn -> 2, jane -> 2, gretchen -> 1, zoe -> 4, manfred -> 1, savitri -> 1, hic
                                                  //| kory -> 3, enzo -> 1, dickory -> 3)
  
 	val likeObinOut: Map[Actor, Int] = zoesTale.outDegrees(LikeObinRelationType)
                                                  //> likeObinOut  : Map[rs.fon.kvizic.networkAnalysis.Actor,Int] = Map(zoe -> 2)
 	 
 	val likeObinIn: Map[Actor, Int] = zoesTale.inDegrees(LikeObinRelationType)
                                                  //> likeObinIn  : Map[rs.fon.kvizic.networkAnalysis.Actor,Int] = Map(hickory -> 
                                                  //| 1, dickory -> 1)
 	
 	val components: List[Network] = Tarjan.connectedComponents(zoesTale)
                                                  //> components  : List[rs.fon.kvizic.networkAnalysis.Network] = List(rs.fon.kviz
                                                  //| ic.networkAnalysis.Network@1ad3ac1, rs.fon.kvizic.networkAnalysis.Network@1e
                                                  //| c56c9, rs.fon.kvizic.networkAnalysis.Network@135d858, rs.fon.kvizic.networkA
                                                  //| nalysis.Network@eac4a)
  for (component <- components){
  	println("Component: \n")
  	for (actor <- component.actors) println(actor + "\n")
  }                                               //> Component: 
                                                  //| 
                                                  //| dickory
                                                  //| 
                                                  //| hickory
                                                  //| 
                                                  //| enzo
                                                  //| 
                                                  //| jane
                                                  //| 
                                                  //| john
                                                  //| 
                                                  //| zoe
                                                  //| 
                                                  //| Component: 
                                                  //| 
                                                  //| manfred
                                                  //| 
                                                  //| gretchen
                                                  //| 
                                                  //| Component: 
                                                  //| 
                                                  //| savitri
                                                  //| 
                                                  //| Component: 
                                                  //| 
                                                  //| betsy
                                                  //| 
```


