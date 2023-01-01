package aoc2022.solutions

object Day15:
  case class Point(x: Int, y: Int)
  case class SensorReading(sensorPosition: Point, closestBeacon: Point)

  def parseSensorReading(input: String): SensorReading =
    input match {
      case s"Sensor at x=$x1, y=$y1: closest beacon is at x=$x2, y=$y2" =>
        SensorReading(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    }

  def parse(input: String): Seq[SensorReading] =
    input.split("\n").map(_.trim).filter(_.nonEmpty).map(parseSensorReading)

@main
def day15Main: Unit =
  import Day15._
  import Day15Input._
  println(parse(input))
