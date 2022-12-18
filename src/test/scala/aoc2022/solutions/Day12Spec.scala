package aoc2022.solutions

import Day12._

class Day12Spec extends munit.FunSuite:
  test("elevation") {
    assertEquals(
      ('a' to 'z').map(_.toString).map(elevationOf),
      0 to 25
    )
    assertEquals(elevationOf("S"), 0)
    assertEquals(elevationOf("E"), 25)
  }
