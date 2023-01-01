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
    def beaconFreeArea: Set[Point] =
      knownArea.filterNot(_ == closestBeacon)

  def parseSensorReading(input: String): Sensor =
    input match {
      case s"Sensor at x=$x1, y=$y1: closest beacon is at x=$x2, y=$y2" =>
        Sensor(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    }

  def parse(input: String): (Int, Seq[Sensor]) =
    val inputLines = input.split("\n").map(_.trim).filter(_.nonEmpty)
    val row = inputLines.headOption.map(_.toInt).getOrElse(0)
    val sensors = inputLines.drop(1).map(parseSensorReading)
    (row, sensors)

  def commonBeaconFreeArea(sensors: Seq[Sensor]): Set[Point] =
    sensors.foldLeft(Set())((set, sensor) =>
      set.union(sensor.beaconFreeArea)
    )

  def sureBeaconFreePlacesInRow(sensors: Seq[Sensor], row: Int): Int =
    val freeArea = commonBeaconFreeArea(sensors)
    freeArea.filter(_.y == row).size

  def solutionPart1(parsedAndRowNumber: (Int, Seq[Sensor])): Int =
    sureBeaconFreePlacesInRow(parsedAndRowNumber._2, parsedAndRowNumber._1)

@main
def day15Main: Unit =
  import Day15._
  import Day15Input._
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))
