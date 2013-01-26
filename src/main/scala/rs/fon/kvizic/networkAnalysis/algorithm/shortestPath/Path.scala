package rs.fon.kvizic.networkAnalysis.algorithm.shortestPath

import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.Relation


protected[shortestPath] class Path(val start: Actor, val path: List[List[Relation]]) {
    
    lazy val end: Actor = path.head.head.endActor
    
    lazy val getWeight: Double = countOverallWeight(path.head)
    
    override def toString: String = start + " ---> " + end
    
    
       
    private def countOverallWeight(rels: List[Relation]): Double = {
      val weights: List[Double] = for (rel <- rels) yield rel.weight
      BigDecimal(weights.sum).setScale(2, BigDecimal.RoundingMode.HALF_UP).doubleValue
    }
    
    def considerPath(newPath: Path): Path = {
      if (isBetter(newPath)) newPath
      else if (isEqual(newPath)) {
        val addedPath = newPath.path ::: path
        new Path(start, addedPath)
      } else this
    }
 
    
    private def isBetter(newPath: Path): Boolean = {
      return canCompare(newPath) && newPath.getWeight < getWeight 
    }
    
    private def canCompare(newPath: Path): Boolean = {
      end == newPath.end
    }
    
    private def isEqual(newPath: Path): Boolean = {
      return canCompare(newPath) && newPath.getWeight == getWeight 
    }
    
    
  }