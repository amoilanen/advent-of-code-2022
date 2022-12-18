package aoc2022.solutions

object Day14:

  case class SimulationState(rockTraces: List[RockTrace], stoppedSand: List[Point])

  case class Point(x: Int, y: Int)
  case class RockTrace(points: List[Point])
  def parse(input: String): List[RockTrace] =
    val rockTraceInputs = input.split("\n").filter(_.nonEmpty).toList
    rockTraceInputs.map(traceInput =>
      val points = traceInput.split("->").map(_.trim).map(pointInput =>
        val Array(x, y) = pointInput.split(",")
        Point(x.toInt, y.toInt)
      ).toList
      RockTrace(points)
    )

@main
def day14Main: Unit =
  import Day14._
  import Day14Input._
  val parsed = parse(input)
  println(parsed)
