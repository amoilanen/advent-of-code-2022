package aoc2022.solutions

import Day11._
import Day11Input._

class Day11Spec extends munit.FunSuite:
  val parsedFunctionTestInputs = 0 to 100

  def assertFunctionEquals(obtained: Long => Long, expected: Long => Long): Unit =
    parsedFunctionTestInputs.foreach(input =>
      assertEquals(obtained(input), expected(input), s"Functions do not match for input $input")
    )

  test("parseMonkeyFunction") {
    assertFunctionEquals(
      parseMonkeyFunction("new = old * 19"),
      (x: Long) => x * 19
    )
    assertFunctionEquals(
      parseMonkeyFunction("new = old + 6"),
      (x: Long) => x + 6
    )
    assertFunctionEquals(
      parseMonkeyFunction("new = old * old"),
      (x: Long) => x * x
    )
    assertFunctionEquals(
      parseMonkeyFunction("new = old + old"),
      (x: Long) => x + x
    )
    assertFunctionEquals(
      parseMonkeyFunction("new = 4 + old"),
      (x: Long) => x + 4
    )
  }

  test("Monkey.inspectItem") {
    val monkeyZero = Monkey(0, List(79L, 98L), (x: Long) => x * 19, 23, 2, 3)
    assertEquals(
      monkeyZero.inspectItem(79L, divisionByThreeReducer),
      (500L, 3)
    )
    assertEquals(
      monkeyZero.inspectItem(98L, divisionByThreeReducer),
      (620L, 3)
    )
    val monkeyTwo = Monkey(2, List(79L, 60L, 97L), (x: Long) => x * x, 13, 1, 3)
    assertEquals(
      monkeyTwo.inspectItem(79L, divisionByThreeReducer),
      (2080L, 1)
    )
    assertEquals(
      monkeyTwo.inspectItem(60L, divisionByThreeReducer),
      (1200L, 3)
    )
  }

  test("evaluateRounds: acceptance test with the data from the tasks description") {
    val monkeys = parse(input)
    assertEquals(
      evaluateRounds(monkeys, divisionByThreeReducer, 1).monkeyItems.toList,
      List(
        List(20L, 23L, 27L, 26L),
        List(2080L, 25L, 167L, 207L, 401L, 1046L),
        List(),
        List()
      )
    )
    assertEquals(
      evaluateRounds(monkeys, divisionByThreeReducer, 2).monkeyItems.toList,
      List(
        List(695L, 10L, 71L, 135L, 350L),
        List(43L, 49L, 58L, 55L, 362L),
        List(),
        List()
      )
    )
    assertEquals(
      evaluateRounds(monkeys, divisionByThreeReducer, 10).monkeyItems.toList,
      List(
        List(91L, 16L, 20L, 98L),
        List(481L, 245L, 22L, 26L, 1092L, 30L),
        List(),
        List()
      )
    )
    assertEquals(
      evaluateRounds(monkeys, divisionByThreeReducer, 20).monkeyItems.toList,
      List(
        List(10L, 12L, 14L, 26L, 34L),
        List(245L, 93L, 53L, 199L, 115L),
        List(),
        List()
      )
    )
  }

  test("operationCountsPart2: test with the data from the task") {
    val monkeys = parse(input)
    assertEquals(
      operationCountsPart2(monkeys, 1),
      List(2L, 4L, 3L, 6L)
    )
    assertEquals(
      operationCountsPart2(monkeys, 20),
      List(99L, 97L, 8L, 103L)
    )
    assertEquals(
      operationCountsPart2(monkeys, 1000),
      List(5204L, 4792L, 199L, 5192L)
    )
    assertEquals(
      operationCountsPart2(monkeys, 5000),
      List(26075L, 23921L, 974L, 26000L)
    )
    assertEquals(
      operationCountsPart2(monkeys, 10000),
      List(52166L, 47830L, 1938L, 52013L)
    )
  }
