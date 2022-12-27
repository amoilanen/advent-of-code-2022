package aoc2022.solutions

import aoc2022.solutions.Day14.{Point, Line, RockTrace, SimulationState, dropSand}

class Day14Spec extends munit.FunSuite:

  test("Line contains") {
    val verticalLine = Line(Point(2, 1), Point(2, 6))
    assert(verticalLine.contains(Point(2, 1)))
    assert(verticalLine.contains(Point(2, 4)))
    assert(verticalLine.contains(Point(2, 6)))
    assert(!verticalLine.contains(Point(1, 1)))
    assert(!verticalLine.contains(Point(2, 10)))
    assert(!verticalLine.contains(Point(10, 12)))

    val horizontalLine = Line(Point(10, 2), Point(12, 2))
    assert(horizontalLine.contains(Point(10, 2)))
    assert(horizontalLine.contains(Point(12, 2)))
    assert(horizontalLine.contains(Point(11, 2)))
    assert(!horizontalLine.contains(Point(9, 1)))
    assert(!horizontalLine.contains(Point(13, 2)))
    assert(!horizontalLine.contains(Point(1, 2)))
  }

  test("RockTrace contains") {
    val rockTrace = RockTrace(List(Point(2, 1), Point(2, 4), Point(7, 4)))
    assert(rockTrace.contains(Point(2, 1)))
    assert(rockTrace.contains(Point(2, 2)))
    assert(rockTrace.contains(Point(2, 4)))
    assert(rockTrace.contains(Point(5, 4)))
    assert(rockTrace.contains(Point(7, 4)))
    assert(!rockTrace.contains(Point(4, 2)))
    assert(!rockTrace.contains(Point(1, 5)))
  }

  test("SimulationState isBlocked") {
    val rockTraces = List(
      RockTrace(List(Point(2, 1), Point(2, 4), Point(7, 4))),
      RockTrace(List(Point(3, 2), Point(5, 2)))
    )
    val stoppedSand = List(Point(3, 1), Point(4, 1), Point(5, 1))
    val simulationState = SimulationState(rockTraces, stoppedSand)
    assert(simulationState.isBlocked(Point(4, 4)))
    assert(simulationState.isBlocked(Point(7, 4)))
    assert(simulationState.isBlocked(Point(4, 1)))
    assert(!simulationState.isBlocked(Point(4, 5)))
    assert(!simulationState.isBlocked(Point(6, 1)))
    assert(!simulationState.isBlocked(Point(4, 3)))
  }

  test("dropSand: rock trace bottom a few points below") {
    val state = SimulationState(List(RockTrace(List(Point(0, 10), Point(5, 10)))))
    assertEquals(
      dropSand(state, Point(2, 0)),
      state.copy(stoppedSand = List(Point(2, 9)))
    )
  }

  test("dropSand: examples similar to examples from the task") {
    //TODO:
  }

  test("dropSand: sand drops infinitely to the bottom") {

  }
