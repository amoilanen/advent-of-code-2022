package aoc2022.solutions

import Day9._

class Day9Spec extends munit.FunSuite:
  test("tailTrail: when trail moves") {
    assertEquals(
      Rope(Point(3, 1), Point(1, 1)).dragTail,
      (Rope(Point(3, 1), Point(2, 1)), TailTrail(Set(Point(1, 1), Point(2, 1))))
    )
    assertEquals(
      Rope(Point(1, 1), Point(1, 3)).dragTail,
      (Rope(Point(1, 1), Point(1, 2)), TailTrail(Set(Point(1, 3), Point(1, 2))))
    )
    assertEquals(
      Rope(Point(2, 3), Point(1, 1)).dragTail,
      (Rope(Point(2, 3), Point(2, 2)), TailTrail(Set(Point(1, 1), Point(2, 2))))
    )
    assertEquals(
      Rope(Point(3, 2), Point(1, 1)).dragTail,
      (Rope(Point(3, 2), Point(2, 2)), TailTrail(Set(Point(1, 1), Point(2, 2))))
    )
  }

  test("tailTrail: when trail does not move") {
    assertEquals(
      Rope(Point(1, 0), Point(0, 0)).dragTail,
      (Rope(Point(1, 0), Point(0, 0)), TailTrail(Set(Point(0, 0))))
    )
    assertEquals(
      Rope(Point(2, 2), Point(1, 1)).dragTail,
      (Rope(Point(2, 2), Point(1, 1)), TailTrail(Set(Point(1, 1))))
    )
  }
