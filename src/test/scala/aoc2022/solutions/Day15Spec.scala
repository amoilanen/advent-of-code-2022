package aoc2022.solutions

import Day15._

class Day15Spec extends munit.FunSuite:

  test("Sensor.beaconFreeAreaAtRow") {
    val sensor = Sensor(Point(0, 0), Point(1, 2))
    assertEquals(
      sensor.coverageAtRow(0),
      Some(HorizontalSegment(0, -3, 3))
    )
    assertEquals(
      sensor.coverageAtRow(2),
      Some(HorizontalSegment(2, -1, 1))
    )
    assertEquals(
      sensor.coverageAtRow(-2),
      Some(HorizontalSegment(-2, -1, 1))
    )
    assertEquals(
      sensor.coverageAtRow(-3),
      Some(HorizontalSegment(-3, 0, 0))
    )
  }

  test("HorizontalSegment.union, does not intersect with existing segments") {
    val x = HorizontalSegment(3, 6, 10)
    val s1 = HorizontalSegment(3, 2, 4)
    val s2 = HorizontalSegment(3, 14, 16)
    assertEquals(
      x.union(Set(s1, s2)),
      Set(s1, x, s2)
    )
  }

  test("HorizontalSegment.union, intersects with exactly one segment but is not contained in it") {
    val x = HorizontalSegment(3, 3, 6)
    val s1 = HorizontalSegment(3, 2, 4)
    val s2 = HorizontalSegment(3, 14, 16)
    assertEquals(
      x.union(Set(s1, s2)),
      Set(HorizontalSegment(3, 2, 6), s2)
    )
  }

  test("HorizontalSegment.union: intersects with two segments and unites them into a single segment") {
    val x = HorizontalSegment(3, 4, 10)
    val s1 = HorizontalSegment(3, 2, 6)
    val s2 = HorizontalSegment(3, 8, 12)
    assertEquals(
      x.union(Set(s1, s2)),
      Set(HorizontalSegment(3, 2, 12))
    )
  }

  test("HorizontalSegment.union: subsumes one of the existing segments") {
    val x = HorizontalSegment(3, 4, 10)
    val s1 = HorizontalSegment(3, 6, 8)
    val s2 = HorizontalSegment(3, 12, 14)
    assertEquals(
      x.union(Set(s1, s2)),
      Set(x, s2)
    )
  }

  test("HorizontalSegment.union: lies fully inside one of the existing segments") {
    val x = HorizontalSegment(3, 7, 9)
    val s1 = HorizontalSegment(3, 6, 10)
    val s2 = HorizontalSegment(3, 12, 14)
    assertEquals(
      x.union(Set(s1, s2)),
      Set(s1, s2)
    )
  }

  test("HorizontalSegment.union: subsumes one of the existing segments and intersects with one of the other segments but does not match that segment") {
    val x = HorizontalSegment(3, 4, 13)
    val s1 = HorizontalSegment(3, 6, 8)
    val s2 = HorizontalSegment(3, 12, 14)
    assertEquals(
      x.union(Set(s1, s2)),
      Set(HorizontalSegment(3, 4, 14))
    )
  }

  test("HorizontalSegment.union: subsumes one of the existing segments and unites two other segments into one") {
    val x = HorizontalSegment(3, 5, 13)
    val s1 = HorizontalSegment(3, 2, 6)
    val s2 = HorizontalSegment(3, 8, 10)
    val s3 = HorizontalSegment(3, 12, 14)
    assertEquals(
      x.union(Set(s1, s2, s3)),
      Set(HorizontalSegment(3, 2, 14))
    )
  }

  test("HorizontalSegment.difference: all segments are contained within") {
    val x = HorizontalSegment(3, 2, 12)
    assertEquals(
      x.difference(Set(HorizontalSegment(3, 4, 6), HorizontalSegment(3, 8, 10))),
      Set(HorizontalSegment(3, 2, 3), HorizontalSegment(3, 7, 7), HorizontalSegment(3, 11, 12))
    )
  }

  test("HorizontalSegment.difference: intersecting with one segment") {
    val x = HorizontalSegment(3, 2, 12)
    assertEquals(
      x.difference(Set(HorizontalSegment(3, -1, 5))),
      Set(HorizontalSegment(3, 6, 12))
    )
  }

  test("HorizontalSegment.difference: subtracting segment from another row") {
    val x = HorizontalSegment(3, 2, 12)
    assertEquals(
      x.difference(Set(HorizontalSegment(2, 4, 8))),
      Set(HorizontalSegment(3, 2, 12))
    )
  }

  test("HorizontalSegment.difference: subtracting intersecting segments") {
    val x = HorizontalSegment(3, 2, 14)
    assertEquals(
      x.difference(Set(HorizontalSegment(3, 4, 8), HorizontalSegment(3, 6, 10))),
      Set(HorizontalSegment(3, 2, 3), HorizontalSegment(3, 11, 14))
    )
  }

  test("HorizontalSegment.difference: subtracting an empty list of segments") {
    val x = HorizontalSegment(3, 2, 14)
    assertEquals(
      x.difference(Set.empty),
      Set(HorizontalSegment(3, 2, 14))
    )
  }

  test("HorizontalSegment.difference: segment larger than current segment") {
    val x = HorizontalSegment(9, 0, 20)
    assertEquals(
      x.difference(Set(HorizontalSegment(9, -1, 23))),
      Set()
    )
  }

  test("HorizontalSegment.difference: left end lies in the segment") {
    val x = HorizontalSegment(2, 2, 12)
    assertEquals(
      x.difference(Set(HorizontalSegment(2, 8, 16))),
      Set(HorizontalSegment(2, 2, 7))
    )
  }

  test("HorizontalSegment.difference: right end lies in the segment") {
    val x = HorizontalSegment(2, 2, 12)
    assertEquals(
      x.difference(Set(HorizontalSegment(2, -1, 4))),
      Set(HorizontalSegment(2, 5, 12))
    )
  }