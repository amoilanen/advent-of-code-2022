package aoc2022.solutions

import Day10._

class Day10Spec extends munit.FunSuite:

  test("instructionsToOperations") {
    assertEquals(
      instructionsToOperations(Seq(
        Noop,
        Add(3),
        Add(-5)
      )),
      List(
        None, None, None, Some(3), None, Some(-5)
      )
    )
    assertEquals(
      instructionsToOperations(Seq(
        Add(1),
        Noop,
        Add(-5),
        Add(6),
        Noop,
        Add(-3)
      )),
      List(
        None, None, Some(1), None, None, Some(-5), None, Some(6), None, None, Some(-3)
      )
    )
    assertEquals(
      instructionsToOperations(Seq(
        Noop,
        Noop,
        Add(1)
      )),
      List(
        None, None, None, None, Some(1)
      )
    )
    assertEquals(
      instructionsToOperations(Seq(
        Noop,
        Noop,
        Noop
      )),
      List(
        None, None, None
      )
    )
  }

  test("registerAtCycle: smaller test") {
    val instructions = Seq(
      Noop,
      Add(2),
      Add(-3)
    )
    assertEquals(registerAtCycle(1, instructions), 1) // Noop
    assertEquals(registerAtCycle(2, instructions), 1) // Add(2)
    assertEquals(registerAtCycle(3, instructions), 1) // Add(2)
    assertEquals(registerAtCycle(4, instructions), 3) // Add(-3) + 2
    assertEquals(registerAtCycle(5, instructions), 3) // Add(-3)
    assertEquals(registerAtCycle(6, instructions), 0) // no further instructions -3
  }

  test("registerAtCycle: example from the task description") {
    val instructions = Seq(
      Noop,
      Add(3),
      Add(-5)
    )
    assertEquals(registerAtCycle(1, instructions), 1) // Noop
    assertEquals(registerAtCycle(2, instructions), 1) // Add(3)
    assertEquals(registerAtCycle(3, instructions), 1) // Add(3)
    assertEquals(registerAtCycle(4, instructions), 4) // Add(-5) +3
    assertEquals(registerAtCycle(5, instructions), 4) // Add(-5)
    assertEquals(registerAtCycle(6, instructions), -1) // no further instructions -5
  }

  test("registerAtCycle: acceptance tests from the task description") {
    import Day10Input._
    val instructions = parse(input)
    assertEquals(registerAtCycle(20, instructions), 21)
    assertEquals(registerAtCycle(60, instructions), 19)
    assertEquals(registerAtCycle(100, instructions), 18)
    assertEquals(registerAtCycle(140, instructions), 21)
    assertEquals(registerAtCycle(180, instructions), 16)
    assertEquals(registerAtCycle(220, instructions), 18)
  }
