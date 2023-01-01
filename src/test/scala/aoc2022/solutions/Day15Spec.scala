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
          Point(0, 3),
      )
    )
  }