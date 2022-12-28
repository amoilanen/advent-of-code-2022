package aoc2022.solutions

import scala.annotation.tailrec

object Day14:

  case class SimulationState(rockTraces: List[RockTrace], stoppedSand: List[Point] = List(), freeFallingSand: Boolean = false):
    def isBlocked(p: Point): Boolean =
      stoppedSand.contains(p) || rockTraces.exists(_.contains(p))
    def isBelowRockTraces(p: Point): Boolean =
      rockTraces.forall(_.isBelow(p))

  case class Point(x: Int, y: Int):
    lazy val movesInPriorityOrder =
      List(Point(x, y + 1), Point(x - 1, y + 1), Point(x + 1, y + 1))

  case class Line(start: Point, end: Point):
    def contains(p: Point): Boolean =
      val Point(x1, y1) = start
      val Point(x2, y2) = end
      // Easy to check that start and end belong to the line with this equation => this is the equation of this line
      p.y * (x2 - x1) == p.x * (y2 - y1) + (y1 * x2 - y2 * x1)

  case class LineSegment(start: Point, end: Point):
    private val line: Line = Line(start, end)
    private val xRange = Math.min(start.x, end.x) to Math.max(start.x, end.x)
    private val yRange = Math.min(start.y, end.y) to Math.max(start.y, end.y)
    def contains(p: Point): Boolean =
      line.contains(p) && xRange.contains(p.x) && yRange.contains(p.y)

  trait RockTrace:
    def contains(p: Point): Boolean
    def isBelow(p: Point): Boolean

  case class FiniteRockTrace(points: List[Point]) extends RockTrace:
    val lines = points.zip(points.tail).map((start, end) => LineSegment(start, end))
    val maxY = points.map(_.y).max
    def contains(p: Point): Boolean =
      lines.exists(_.contains(p))
    def isBelow(p: Point): Boolean =
      p.y > maxY

  case class Floor(y: Int) extends RockTrace:
    def contains(p: Point): Boolean =
      p.y == y
    def isBelow(p: Point): Boolean =
      p.y > y

  def parse(input: String): List[FiniteRockTrace] =
    val rockTraceInputs = input.split("\n").filter(_.nonEmpty).toList
    rockTraceInputs.map(traceInput =>
      val points = traceInput.split("->").map(_.trim).map(pointInput =>
        val Array(x, y) = pointInput.split(",")
        Point(x.toInt, y.toInt)
      ).toList
      FiniteRockTrace(points)
    )

  @tailrec
  def dropSand(state: SimulationState, fromPoint: Point): SimulationState =
    fromPoint.movesInPriorityOrder.find(!state.isBlocked(_)) match {
      case Some(nextPoint) =>
        if state.isBelowRockTraces(nextPoint) then
          state.copy(freeFallingSand = true)
        else
          dropSand(state, nextPoint)
      case None => state.copy(stoppedSand = state.stoppedSand :+ fromPoint)
    }

  val SandDropStartPoint = Point(500, 0)

  def solutionPart1(rockTraces: List[FiniteRockTrace]): Int =
    @tailrec
    def findStateInWhichSandFalls(state: SimulationState): SimulationState =
      if state.freeFallingSand then
        state
      else
        val nextState = dropSand(state, SandDropStartPoint)
        findStateInWhichSandFalls(nextState)

    val initialState = SimulationState(rockTraces)
    val finalState = findStateInWhichSandFalls(initialState)
    finalState.stoppedSand.size

  def solutionPart2(rockTraces: List[FiniteRockTrace]): Int =
    @tailrec
    def findStateInWhichSandBlocksSource(state: SimulationState): SimulationState =
      if state.stoppedSand.contains(SandDropStartPoint) then
        state
      else
        val nextState = dropSand(state, SandDropStartPoint)
        findStateInWhichSandBlocksSource(nextState)

    val maxRockTraceY = rockTraces.flatMap(_.points).map(_.y).max
    val initialState = SimulationState(rockTraces :+ Floor(maxRockTraceY + 2))
    val finalState = findStateInWhichSandBlocksSource(initialState)
    finalState.stoppedSand.size

@main
def day14Main: Unit =
  import Day14._
  import Day14Input._
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))
