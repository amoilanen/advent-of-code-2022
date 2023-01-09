package aoc2022.solutions

import Day15._

class Day15Spec extends munit.FunSuite:
  test("Sensor.knownArea") {
    val sensor = Sensor(Point(0, 0), Point(1, 2))
    assertEquals(
      sensor.knownArea,
      Set(Point(0, -3),
          Point(-1, -2), Point(0, -2), Point(1, -2),
          Point(-2, -1), Point(-1, -1), Point(0, -1), Point(1, -1), Point(2, -1),
          Point(-3, 0), Point(-2, 0), Point(-1, 0), Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0),
          Point(-2, 1), Point(-1, 1), Point(0, 1), Point(1, 1), Point(2, 1),
          Point(-1, 2), Point(0, 2), Point(1, 2),
          Point(0, 3)
      )
    )
  }

  test("Sensor.beaconFreeAreaAtRow") {
    val sensor = Sensor(Point(0, 0), Point(1, 2))
    assertEquals(
      sensor.beaconFreeAreaAtRow(0),
      Set(Point(-3, 0), Point(-2, 0), Point(-1, 0), Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0))
    )
    assertEquals(
      sensor.beaconFreeAreaAtRow(2),
      Set(Point(-1, 2), Point(0, 2))
    )
    assertEquals(
      sensor.beaconFreeAreaAtRow(-2),
      Set(Point(-1, -2), Point(0, -2), Point(1, -2))
    )
    assertEquals(
      sensor.beaconFreeAreaAtRow(-3),
      Set(Point(0, -3))
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