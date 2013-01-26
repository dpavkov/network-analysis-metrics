package rs.fon.kvizic.networkAnalysis.algorithm.shortestPath

import rs.fon.kvizic.networkAnalysis.Network
import rs.fon.kvizic.networkAnalysis.Actor
import rs.fon.kvizic.networkAnalysis.Relation


object ShortestPath {
  
  def getPaths(network: Network): Map[Actor, List[Path]] = {
    val paths = for (actor: Actor <- network.actors) 
      yield (actor, getPathsForActor(actor, network))
    paths.toMap
  }
  
  private def getPathsForActor(start: Actor, network: Network): List[Path] = {
    
    def getPathsForActorIter(current: Actor, visited: List[Path], pathToCurrent: Option[Path]): List[Path] = {
      
      def considerRelation(visitedPaths: List[Path], relation: Relation): List[Path] = {
        if (relation.endActor == start) visitedPaths
        else considerNewRelation(visitedPaths, relation)
      }
      
      def considerNewRelation(visitedPaths: List[Path], relation: Relation): List[Path] = {
        val newPath = createNewPath(relation)
        getPreviousPath(visitedPaths, relation) match {
          case None =>  {
            getPathsForActorIter(relation.endActor, newPath :: visitedPaths, Some(newPath))
          }
          case Some(path) => replacePath(visitedPaths, path.considerPath(newPath))
        }
      }
      
      def createNewPath(relation: Relation): Path = {
        pathToCurrent match {
          case None => new Path(start, List[List[Relation]] (List[Relation](relation)))
          case Some(path) => new Path(start, for (relations <- path.path) yield relation :: relations)
        }
      }
      
      current.getAllRelations.foldLeft(visited)((visitedPaths, relation) => considerRelation(visitedPaths, relation))
    }

    val allRelationsForStart = start.getAllRelations
    if (allRelationsForStart.isEmpty) {
      List[Path]()
    } else {
      val firstRel = start.getAllRelations.head
      val firstPath = new Path(start, List[List[Relation]](List[Relation](firstRel)))
      getPathsForActorIter(start, List[Path](), None)
    }
    
  }
  
  private def getPreviousPath(visitedPaths: List[Path], relation: Relation): Option[Path] = {
    if (visitedPaths.isEmpty) None
    else if (visitedPaths.head.end == relation.endActor) 
      Some(visitedPaths.head)
    else getPreviousPath(visitedPaths.tail, relation)
  }

  private def replacePath(paths: List[Path], replacement: Path): List[Path] = {
    for (path <- paths) 
      yield 
      	if (path.end == replacement.end)
         replacement
        else  path
  }
}

  