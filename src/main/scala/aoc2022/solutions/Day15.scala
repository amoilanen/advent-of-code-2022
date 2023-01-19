package aoc2022.solutions

import scala.annotation.tailrec

object Day15:
  case class Point(x: Int, y: Int):
    def distanceTo(other: Point): Int =
      Math.abs(x - other.x) + Math.abs(y - other.y)

  case class HorizontalSegment(row: Int, start: Int, end: Int):
    val size = Math.abs(end - start + 1)

    def contains(p: Point): Boolean =
      p.y == row && p.x >= start && p.x <= end

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

    enum Direction:
      case Left, Right
    case class HorizontalSegmentEnd(x: Int, direction: Direction)

    def difference(other: Set[HorizontalSegment]): Set[HorizontalSegment] =
      // Initial implementation
      // TODO: Re-factor the implementation, debug, handle corner cases
      val segmentEnds = other.toList.flatMap(segment =>
        val HorizontalSegment(_, left, right) = segment
        List(HorizontalSegmentEnd(left, Direction.Left), HorizontalSegmentEnd(right, Direction.Right))
      ).sortBy(_.x)
      val fullSegmentEnds = segmentEnds.zip(segmentEnds.tail).map(pair => List(pair._1, pair._2)).flatMap(group =>
        group match
          case List(HorizontalSegmentEnd(first, Direction.Left), HorizontalSegmentEnd(second, Direction.Left)) =>
            group :+ HorizontalSegmentEnd(second - 1, Direction.Right)
          case List(HorizontalSegmentEnd(first, Direction.Right), HorizontalSegmentEnd(second, Direction.Right)) =>
            group :+ HorizontalSegmentEnd(second - 1, Direction.Left)
          case _ =>
            group
      ).toSet.toList.sortBy(_.x)

      val possibleSegments = fullSegmentEnds.zip(fullSegmentEnds.tail).map(pair => HorizontalSegment(row, pair._1.x, pair._2.x))
      val remainingSegments = possibleSegments.filter(segment =>
        !other.exists(_.contains(Point(row, segment.start)))
      )
      remainingSegments.toSet

  case class Sensor(position: Point, closestBeacon: Point):
    lazy val radius = position.distanceTo(closestBeacon)

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
    val horizontalSegments = sensors.foldLeft(Set.empty[HorizontalSegment])((set, sensor) =>
      sensor.coverageAtRow(row).map(_.union(set)).getOrElse(set)
    )
    val coveredBySensors = horizontalSegments.map(_.size).sum
    val beaconsCovered = sensors.map(_.closestBeacon).toSet.filter(beacon =>
      horizontalSegments.exists(_.contains(beacon))).size
    coveredBySensors - beaconsCovered

  def solutionPart1(parsedAndRowNumber: (Int, Seq[Sensor])): Int =
    val (rowNumber, sensors) = parsedAndRowNumber
    sureBeaconFreePlacesInRow(parsedAndRowNumber._2, parsedAndRowNumber._1)

  def solutionPart2(parsedAndRowNumber: (Int, Seq[Sensor])): Int =
    val (rowNumber, sensors) = parsedAndRowNumber
    val possibleRange = rowNumber * 2
    ???

@main
def day15Main: Unit =
  import Day15._
  import Day15Input._
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))
