package aoc2022.solutions

import aoc2022.solutions.common.ParsingUtils.ParsingError

object Day12:

  case class Vertex(mark: String, elevation: Int, row: Int, column: Int):
    def isWithin(columnsNumber: Int, rowNumber: Int): Boolean =
      row >= 0 && row < rowNumber && column >= 0 && column < columnsNumber

  case class Edge(from: Vertex, to: Vertex)
  case class Graph(vertices: Array[Array[Vertex]], start: Vertex, end: Vertex):

    val columnsNumber: Int = vertices.size
    val rowNumber: Int = vertices.headOption.map(_.size).getOrElse(0)

    def neighbors(ofVertex: Vertex): Seq[Vertex] =
      (-1 to 1).flatMap({ rowIncrement =>
        (-1 to 1).map({ columnIncrement =>
          vertices(ofVertex.row + rowIncrement)(ofVertex.column + columnIncrement)
        })
        }).filter(p =>
        p != ofVertex && p.isWithin(columnsNumber, rowNumber)
      )

    def edgesFrom(ofVertex: Vertex): Seq[Edge] =
      neighbors(ofVertex).filter(neighbor =>
        val neighborElevation = neighbor.elevation
        neighborElevation == ofVertex.elevation || neighborElevation == ofVertex.elevation + 1
      ).map(Edge(ofVertex, _))

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
    val end = allVertices.find(_.mark == "S").getOrElse(throw new ParsingError("Did not find the ending square"))
    Graph(vertices, start, end)

@main
def day12Main: Unit =
  import Day12._
  import Day12Input._
  val parsed = parse(input)
  println(parsed)