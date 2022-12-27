package aoc2022.solutions

import scala.annotation.tailrec

object Day14:

  case class SimulationState(rockTraces: List[RockTrace], stoppedSand: List[Point] = List(), freeFallingSand: Boolean = false):
    def isBlocked(p: Point): Boolean =
      stoppedSand.contains(p) || rockTraces.exists(_.contains(p))

  case class Point(x: Int, y: Int):
    lazy val movesInPriorityOrder =
      List(Point(x, y + 1), Point(x - 1, y + 1), Point(x + 1, y + 1))

  case class Line(start: Point, end: Point):
    private val minX = Math.min(start.x, end.x)
    private val maxX = Math.max(start.x, end.x)
    private val minY = Math.min(start.y, end.y)
    private val maxY = Math.max(start.y, end.y)
    def contains(p: Point): Boolean =
      val Point(x1, y1) = start
      val Point(x2, y2) = end
      // Easy to check that start and end belong to the line with this equation => this is the equation of this line
      p.y * (x2 - x1) == p.x * (y2 - y1) + (y1 * x2 - y2 * x1)
      &&
        ((p.x >= minX) && (p.x <= maxX))
      &&
        ((p.y >= minY) && (p.y <= maxY))

  case class RockTrace(points: List[Point]):
    val lines = points.zip(points.tail).map((start, end) => Line(start, end))
    def contains(p: Point): Boolean =
      lines.exists(_.contains(p))

  def parse(input: String): List[RockTrace] =
    val rockTraceInputs = input.split("\n").filter(_.nonEmpty).toList
    rockTraceInputs.map(traceInput =>
      val points = traceInput.split("->").map(_.trim).map(pointInput =>
        val Array(x, y) = pointInput.split(",")
        Point(x.toInt, y.toInt)
      ).toList
      RockTrace(points)
    )

  @tailrec
  def dropSand(state: SimulationState, fromPoint: Point): SimulationState =
    fromPoint.movesInPriorityOrder.find(!state.isBlocked(_)) match {
      case Some(nextPoint) => dropSand(state, nextPoint)
      case None => state.copy(stoppedSand = state.stoppedSand :+ fromPoint)
    }

@main
def day14Main: Unit =
  import Day14._
  import Day14Input._
  val parsed = parse(input)
  println(parsed)
