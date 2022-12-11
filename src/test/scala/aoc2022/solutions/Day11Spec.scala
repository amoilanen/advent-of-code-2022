package aoc2022.solutions

import Day11._

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
