package aoc2022.solutions

import aoc2022.solutions.Day12.Edge
import aoc2022.solutions.common.ParsingUtils.ParsingError

import scala.annotation.tailrec

object Day12:

  case class Point(row: Int, column: Int):
    def isWithin(rowNumber: Int, columnsNumber: Int): Boolean =
      row >= 0 && row < rowNumber && column >= 0 && column < columnsNumber

  case class Vertex(mark: String, elevation: Int, row: Int, column: Int):
    val point: Point = Point(row, column)

  case class Edge(from: Vertex, to: Vertex):
    val length: Int = 1

  case class Graph(vertices: Array[Array[Vertex]], start: Vertex, end: Vertex):
    val allVertices = vertices.flatten
    val rowNumber: Int = vertices.size
    val columnNumber: Int = vertices.headOption.map(_.size).getOrElse(0)

    private val MovementDirections: List[(Int, Int)] =
      List((-1, 0), (1, 0), (0, -1), (0, 1))

    def neighbors(ofVertex: Vertex): Seq[Vertex] =
      MovementDirections.map((rowIncrement, columnIncrement) =>
        Point(ofVertex.row + rowIncrement, ofVertex.column + columnIncrement)
      ).filter(p =>
        p != ofVertex.point && p.isWithin(rowNumber, columnNumber)
      ).map(p =>
        vertices(p.row)(p.column)
      )

    def edgesFrom(ofVertex: Vertex): Seq[Edge] =
      neighbors(ofVertex).filter(neighbor =>
        val neighborElevation = neighbor.elevation
        neighborElevation == ofVertex.elevation || neighborElevation == ofVertex.elevation + 1
      ).map(Edge(ofVertex, _))

    /*
     * Implementation of the Dijkstra's algorithm https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
     */
    def findShortestPath(pathStart: Vertex, pathEnd: Vertex): List[Edge] =
      val initialDistances: Map[Vertex, Int] = allVertices.map(_ -> Int.MaxValue).toList.toMap.updated(pathStart, 0)
      val initialBackreferences: Map[Vertex, Option[Vertex]] = allVertices.map(_ -> None).toList.toMap

      @tailrec
      def algorithmIteration(distances: Map[Vertex, Int], backreferences: Map[Vertex, Option[Vertex]], remaining: List[Vertex]): (Int, List[Edge]) =
        val remainingSortedByDistance = remaining.sortBy(distances(_))
        remainingSortedByDistance.headOption match {
          case None =>
            val distanceToPathEnd = distances(pathEnd)
            val path = buildPathTo(pathEnd, backreferences)
            (distanceToPathEnd, path)
          case Some(nextClosestVertex) =>
            val updated = edgesFrom(nextClosestVertex).foldLeft((distances, backreferences))({
              case ((currentDistances, currentBackreferences), edge) =>
                val Edge(_, toVertex) = edge
                val alternativeDistance = distances(nextClosestVertex) + edge.length
                if (alternativeDistance < distances(toVertex))
                  (currentDistances.updated(toVertex, alternativeDistance), currentBackreferences.updated(toVertex, Some(nextClosestVertex)))
                else
                  (currentDistances, currentBackreferences)
            })
            val (updatedDistances, updatedBackreferences) = updated
            val updatedRemaining = if (nextClosestVertex == pathEnd)
              List.empty
            else
              remainingSortedByDistance.tail
            algorithmIteration(updatedDistances, updatedBackreferences, updatedRemaining)
        }

      def buildPathTo(to: Vertex, backreferences: Map[Vertex, Option[Vertex]]): List[Edge] =
        @tailrec
        def iteration(current: Vertex, builtPath: List[Edge]): List[Edge] =
          backreferences.get(current).flatten match {
            case Some(edgeStart) =>
              val edge = Edge(edgeStart, current)
              iteration(edgeStart, edge +: builtPath)
            case None =>
              builtPath
          }

        iteration(to, List())

      algorithmIteration(initialDistances, initialBackreferences, allVertices.toList)._2

    override def toString: String =
      val verticesRepresentation = vertices.map(
        _.toList
      ).toList.toString
      s"""
        |Graph[$verticesRepresentation]
        |""".stripMargin

  def elevationOf(square: String): Int =
    square match {
      case "S" => 0
      case "E" => 25
      case _ => square.charAt(0).toInt - 'a'.toInt
    }

  def parse(input: String): Graph =
    val graphInput: Array[Array[String]] = input.split("\n").map(_.trim).filter(_.nonEmpty).map(row =>
      row.split("").filter(_.nonEmpty)
    )
    val vertices = graphInput.zipWithIndex.map({ case (row, rowIndex) =>
      row.zipWithIndex.map({ case (mark, columnIndex) =>
        Vertex(mark, elevationOf(mark), rowIndex, columnIndex)
      })
    })
    val allVertices = vertices.flatten
    val start = allVertices.find(_.mark == "S").getOrElse(throw new ParsingError("Did not find the starting square"))
    val end = allVertices.find(_.mark == "E").getOrElse(throw new ParsingError("Did not find the ending square"))
    Graph(vertices, start, end)

  def solutionPart1(graph: Graph): Int =
    val shortestPath = graph.findShortestPath(graph.start, graph.end)
    println(shortestPath)
    shortestPath.size

@main
def day12Main: Unit =
  import Day12._
  import Day12Input._
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))