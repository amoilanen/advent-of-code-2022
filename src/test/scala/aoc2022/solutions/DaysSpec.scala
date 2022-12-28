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

  test("Day 2") {
    import Day2._
    import Day2Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 15)
    assertEquals(solutionPart2(parsed), 12)
  }

  test("Day 3") {
    import Day3._
    import Day3Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 157)
    assertEquals(solutionPart2(parsed), 70)
  }

  test("Day 4") {
    import Day4._
    import Day4Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 2)
    assertEquals(solutionPart2(parsed), 4)
  }

  test("Day 5") {
    import Day5._
    import Day5Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), "CMZ")
    assertEquals(solutionPart2(parsed), "MCD")
  }

  test("Day 6") {
    import Day6._
    import Day6Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 7)
    assertEquals(solutionPart2(parsed), 19)
  }

  test("Day 7") {
    import Day7._
    import Day7Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 95437)
    assertEquals(solutionPart2(parsed), 24933642)
  }

  test("Day 8") {
    import Day8._
    import Day8Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 21)
    assertEquals(solutionPart2(parsed), 8)
  }

  test("Day 9") {
    import Day9._
    import Day9Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 88)
    assertEquals(solutionPart2(parsed), 36)
  }

  test("Day 10") {
    import Day10._
    import Day10Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 13140)
    assertEquals(solutionPart2(parsed),
      """##..##..##..##..##..##..##..##..##..##..
        |###...###...###...###...###...###...###.
        |####....####....####....####....####....
        |#####.....#####.....#####.....#####.....
        |######......######......######......####
        |#######.......#######.......#######.....""".stripMargin)
  }

  test("Day 11") {
    import Day11._
    import Day11Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 10605L)
    assertEquals(solutionPart2(parsed), 2713310158L)
  }

  test("Day 12") {
    import Day12._
    import Day12Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 31)
    assertEquals(solutionPart2(parsed), 29)
  }

  test("Day 13") {
    import Day13._
    import Day13Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 13)
    assertEquals(solutionPart2(parsed), 140)
  }

  test("Day 14") {
    import Day14._
    import Day14Input._
    val parsed = parse(input)
    assertEquals(solutionPart1(parsed), 24)
    assertEquals(solutionPart2(parsed), 93)
  }
