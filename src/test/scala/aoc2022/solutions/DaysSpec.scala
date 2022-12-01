package aoc2022.solutions

import aoc2022.solutions._

class DaysSpec extends munit.FunSuite:

  test("Day 1") {
    import Day1._
    import Day1Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 24000)
    assertEquals(solutionPart2(parsed), 45000)
  }