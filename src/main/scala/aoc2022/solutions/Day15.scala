package aoc2022.solutions

import scala.annotation.tailrec

object Day15:
  case class Point(x: Int, y: Int):
    def distanceTo(other: Point): Int =
      Math.abs(x - other.x) + Math.abs(y - other.y)

  case class HorizontalSegment(row: Int, start: Int, end: Int):
    lazy val asPointSet: Set[Point] =
      (start to end).map(Point(_, row)).toSet

    def intersectsWith(other: HorizontalSegment): Boolean =
      row == other.row && (
        Seq(other.start, other.end).exists(x => x >= start && x <= end)
          || ((other.start < this.start) && (this.end < other.end)))

    def mergeWith(other: HorizontalSegment): HorizontalSegment =
      if this.intersectsWith(other) then
        val ends = Seq(start, end, other.start, other.end)
        HorizontalSegment(row, ends.min, ends.max)
      else
        this

    final def union(other: Set[HorizontalSegment]): Set[HorizontalSegment] =
      /*
       * Idea of the algorithm, s - current segment:
       *  1. Find any first segment with which there is an intersection from the set of `other`, if none, then return union of sets
       *  2. Compute a new segment which this intersection produces, s := this intersection,
       *  3. Remove the segment found in 1. from `other`, go to step 1.
       *
       * Since there is a finite number of segments and on step 1 the number of segments gets reduced the algorithm will finish
       * in a finite number of steps.
       */
      def unionOf(current: HorizontalSegment, other: Set[HorizontalSegment]): Set[HorizontalSegment] =
        val segmentWhichIntersects = other.find(segment => current.intersectsWith(segment))
        segmentWhichIntersects match
          case None =>
            other + current
          case Some(segment) =>
            val merged = current.mergeWith(segment)
            val remainingSegments = other.filterNot(_ == segment)
            unionOf(merged, remainingSegments)
      unionOf(this, other)


    def difference(lines: Set[HorizontalSegment]): Set[HorizontalSegment] =
      ???

  case class Sensor(position: Point, closestBeacon: Point):
    lazy val radius = position.distanceTo(closestBeacon)
    def knownArea: Set[Point] =
      (-radius to radius).flatMap(rowOffset =>
        val sectionLength = radius - Math.abs(rowOffset)
        (-sectionLength to sectionLength).map(columnOffset =>
          Point(position.x + columnOffset, position.y + rowOffset)
        )
      ).toSet

    def beaconFreeAreaAtRow(row: Int): Set[Point] =
      coverageAtRow(row) match {
        case Some(line) => line.asPointSet.filterNot(_ == closestBeacon)
        case None => Set()
      }

    def coverageAtRow(row: Int): Option[HorizontalSegment] =
      val verticalOffset = Math.abs(position.y - row)
      if verticalOffset <= radius then
        val sectionLength = radius - verticalOffset
        Some(HorizontalSegment(row, position.x - sectionLength, position.x + sectionLength))
      else
        None

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

  def sureBeaconFreePlacesInRow(sensors: Seq[Sensor], row: Int): Int =
    sensors.foldLeft(Set.empty[Point])((set, sensor) =>
      set.union(sensor.beaconFreeAreaAtRow(row))
    ).size

  def solutionPart1(parsedAndRowNumber: (Int, Seq[Sensor])): Int =
    sureBeaconFreePlacesInRow(parsedAndRowNumber._2, parsedAndRowNumber._1)

  def solutionPart2(sensors: Seq[Sensor]): Int =
    ???

@main
def day15Main: Unit =
  import Day15._
  import Day15Input._
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))
