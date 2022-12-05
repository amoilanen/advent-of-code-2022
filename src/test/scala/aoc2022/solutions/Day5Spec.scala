package aoc2022.solutions

import Day5._

class Day5Spec extends munit.FunSuite:
  test("parseCrossStackCut") {
    assertEquals(parseCrossStackCut("    [D]    "), List(None, Some("D"), None))
    assertEquals(parseCrossStackCut("[N] [C]    "), List(Some("N"), Some("C"), None))
    assertEquals(parseCrossStackCut("[Z] [M] [P]"), List(Some("Z"), Some("M"), Some("P")))
  }

  test("parseStacksOfCrates: no trailing spaces") {
    val input = """
      |    [D]
      |[N] [C]
      |[Z] [M] [P]
      | 1   2   3
      |""".stripMargin
    val parsed = parseStacksOfCrates(input.split("\n"))
    assertEquals(parsed,
      StacksOfCrates(List(List("N", "Z"), List("D", "C", "M"), List("P"))))
  }

  test("parseStacksOfCrates: trailing spaces") {
    val input =
      """
        |    [D]
        |[N] [C]
        |[Z] [M] [P]
        | 1   2   3
        |""".stripMargin
    val parsed = parseStacksOfCrates(input.split("\n"))
    assertEquals(parsed,
      StacksOfCrates(List(List("N", "Z"), List("D", "C", "M"), List("P"))))
  }

  test("parseStacksOfCrates: larger input") {
    val input =
      """
        |[H]                 [Z]         [J]
        |[L]     [W] [B]     [G]         [R]
        |[R]     [G] [S]     [J] [H]     [Q]
        |[F]     [N] [T] [J] [P] [R]     [F]
        |[B]     [C] [M] [R] [Q] [F] [G] [P]
        |[C] [D] [F] [D] [D] [D] [T] [M] [G]
        |[J] [C] [J] [J] [C] [L] [Z] [V] [B]
        |[M] [Z] [H] [P] [N] [W] [P] [L] [C]
        | 1   2   3   4   5   6   7   8   9
        |""".stripMargin
    val parsed = parseStacksOfCrates(input.split("\n"))
    assertEquals(parsed,
      StacksOfCrates(
        List(
          List("H", "L", "R", "F", "B", "C", "J", "M"),
          List("D", "C", "Z"),
          List("W", "G", "N", "C", "F", "J", "H"),
          List("B", "S", "T", "M", "D", "J", "P"),
          List("J", "R", "D", "C", "N"),
          List("Z", "G", "J", "P", "Q", "D", "L", "W"),
          List("H", "R", "F", "T", "Z", "P"),
          List("G", "M", "V", "L"),
          List("J", "R", "Q", "F", "P", "G", "B", "C"),
        )))
  }

  test("parseOperations") {
    val input =
      """
        |move 1 from 2 to 1
        |move 3 from 1 to 3
        |move 2 from 2 to 1
        |move 1 from 1 to 2
        |""".stripMargin
    val parsed = parseOperations(input.split("\n").filter(_.trim.nonEmpty).toList)
    assertEquals(parsed,
      List(
        Operation(1, 2, 1),
        Operation(3, 1, 3),
        Operation(2, 2, 1),
        Operation(1, 1, 2)
      ))
  }

  test("applyOperation: step 1") {
    val stacks = StacksOfCrates(
      List(
        List("N", "Z"),
        List("D", "C", "M"),
        List("P")
      )
    )
    val expectedStacksAfter = StacksOfCrates(
      List(
        List("D", "N", "Z"),
        List("C", "M"),
        List("P")
      )
    )
    assertEquals(applyOperation(stacks, Operation(1, 2, 1)), expectedStacksAfter)
  }

  test("applyOperation: step 2") {
    val stacks = StacksOfCrates(
      List(
        List("D", "N", "Z"),
        List("C", "M"),
        List("P")
      )
    )
    val expectedStacksAfter = StacksOfCrates(
      List(
        List(),
        List("C", "M"),
        List("Z", "N", "D", "P")
      )
    )
    assertEquals(applyOperation(stacks, Operation(3, 1, 3)), expectedStacksAfter)
  }

  test("applyOperation: step 3") {
    val stacks = StacksOfCrates(
      List(
        List(),
        List("C", "M"),
        List("Z", "N", "D", "P")
      )
    )
    val expectedStacksAfter = StacksOfCrates(
      List(
        List("M", "C"),
        List(),
        List("Z", "N", "D", "P")
      )
    )
    assertEquals(applyOperation(stacks, Operation(2, 2, 1)), expectedStacksAfter)
  }

  test("applyOperation: step 4") {
    val stacks = StacksOfCrates(
      List(
        List("M", "C"),
        List(),
        List("Z", "N", "D", "P")
      )
    )
    val expectedStacksAfter = StacksOfCrates(
      List(
        List("C"),
        List("M"),
        List("Z", "N", "D", "P")
      )
    )
    assertEquals(applyOperation(stacks, Operation(1, 1, 2)), expectedStacksAfter)
  }

