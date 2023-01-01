package aoc2022.solutions

object Day15:
  case class Point(x: Int, y: Int):
    def distanceTo(other: Point): Int =
      Math.abs(x - other.x) + Math.abs(y - other.y)

  case class Sensor(position: Point, closestBeacon: Point):
    def knownArea: Set[Point] =
      val radius = position.distanceTo(closestBeacon)
      (-radius to radius).flatMap(rowOffset =>
        val sectionLength = radius - Math.abs(rowOffset)
        (-sectionLength to sectionLength).map(columnOffset =>
          Point(position.x + columnOffset, position.y + rowOffset)
        )
      ).toSet

  def parseSensorReading(input: String): Sensor =
    input match {
      case s"Sensor at x=$x1, y=$y1: closest beacon is at x=$x2, y=$y2" =>
        Sensor(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    }

  def parse(input: String): Seq[Sensor] =
    input.split("\n").map(_.trim).filter(_.nonEmpty).map(parseSensorReading)

@main
def day15Main: Unit =
  import Day15._
  import Day15Input._
  println(parse(input))
