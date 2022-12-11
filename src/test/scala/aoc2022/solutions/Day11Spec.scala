package aoc2022.solutions

import Day11._
import Day11Input._

class Day11Spec extends munit.FunSuite:
  val parsedFunctionTestInputs = 0 to 100

  def assertFunctionEquals(obtained: Int => Int, expected: Int => Int): Unit =
    parsedFunctionTestInputs.foreach(input =>
      assertEquals(obtained(input), expected(input), s"Functions do not match for input $input")
    )

  test("parseMonkeyFunction") {
    assertFunctionEquals(
      parseMonkeyFunction("new = old * 19"),
      (x: Int) => x * 19
    )
    assertFunctionEquals(
      parseMonkeyFunction("new = old + 6"),
      (x: Int) => x + 6
    )
    assertFunctionEquals(
      parseMonkeyFunction("new = old * old"),
      (x: Int) => x * x
    )
    assertFunctionEquals(
      parseMonkeyFunction("new = old + old"),
      (x: Int) => x + x
    )
    assertFunctionEquals(
      parseMonkeyFunction("new = 4 + old"),
      (x: Int) => x + 4
    )
  }

  test("Monkey.inspectItem") {
    val monkeyZero = Monkey(0, List(79, 98), (x: Int) => x * 19, 23, 2, 3)
    assertEquals(
      monkeyZero.inspectItem(79, Part1WorryLevelReducer),
      (500, 3)
    )
    assertEquals(
      monkeyZero.inspectItem(98, Part1WorryLevelReducer),
      (620, 3)
    )
    val monkeyTwo = Monkey(2, List(79, 60, 97), (x: Int) => x * x, 13, 1, 3)
    assertEquals(
      monkeyTwo.inspectItem(79, Part1WorryLevelReducer),
      (2080, 1)
    )
    assertEquals(
      monkeyTwo.inspectItem(60, Part1WorryLevelReducer),
      (1200, 3)
    )
  }

  test("evaluateRounds: acceptance test with the ") {
    val monkeys = parse(input)
    assertEquals(
      evaluateRounds(monkeys, Part1WorryLevelReducer, 1).monkeyItems.toList,
      List(
        List(20, 23, 27, 26),
        List(2080, 25, 167, 207, 401, 1046),
        List(),
        List()
      )
    )
    assertEquals(
      evaluateRounds(monkeys, Part1WorryLevelReducer, 2).monkeyItems.toList,
      List(
        List(695, 10, 71, 135, 350),
        List(43, 49, 58, 55, 362),
        List(),
        List()
      )
    )
    assertEquals(
      evaluateRounds(monkeys, Part1WorryLevelReducer, 10).monkeyItems.toList,
      List(
        List(91, 16, 20, 98),
        List(481, 245, 22, 26, 1092, 30),
        List(),
        List()
      )
    )
    assertEquals(
      evaluateRounds(monkeys, Part1WorryLevelReducer, 20).monkeyItems.toList,
      List(
        List(10, 12, 14, 26, 34),
        List(245, 93, 53, 199, 115),
        List(),
        List()
      )
    )
  }
