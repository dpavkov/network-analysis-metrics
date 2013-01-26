package rs.fon.kvizic.networkAnalysis.algorithm.betweenness

import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.ShortestPath
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.algorithm.shortestPath.Path

import scala.Double

class Betweenness(shortestPath: ShortestPath){

  def getBetweennessValue(): Map[Actor, Double] = {
    shortestPath.getPaths.foldLeft (Map[Actor, Double]()) ( (acc: Map[Actor, Double], paths : (Actor, List[Path])) => considerElements(acc, getMiddleElements(paths._2), paths._2.size))
  }
  
  private def getMiddleElements(paths : List[Path]): Map[Actor, Double] = {
    val middleElements: List[Actor] = 
      for(path <- paths;
        singlePath <- path.path;
        rel <- singlePath if !(rel.endActor == path.end) )
      yield rel.endActor
    countOccurences(middleElements)
  }
  
  private def countOccurences(elements: List[Actor]): Map[Actor, Double] = {
    elements.foldLeft(Map[Actor, Double]())((occurs, elem) => addOrIncrement(occurs, elem))
  }
  
  private def addOrIncrement(map: Map[Actor, Double], elem: Actor) : Map[Actor, Double] = {
	addOrIncrease(map, elem, 1.0)
  }
  
  private def addOrIncrease(map: Map[Actor, Double], elem: Actor, addend: Double): Map[Actor, Double] = {
    map.get(elem) match {
      case None => map + (elem -> addend)
      case Some(prevVal : Double) => map.updated(elem , prevVal + addend)
    }
  }
  
  private def considerElements( acc: Map[Actor, Double], middleElements: Map[Actor, Double], pathsCount: Int) : Map[Actor, Double] = {
    middleElements.foldLeft(acc)((acc, actorOccurences) => addOrIncrease(acc, actorOccurences._1, actorOccurences._2 / pathsCount))
  }
}