package aoc2022.solutions

import aoc2022.solutions.Day14.{FiniteRockTrace, Floor, Line, LineSegment, Point, RockTrace, SimulationState, dropSand}

class Day14Spec extends munit.FunSuite:

  test("LineSegment contains") {
    val verticalSegment = LineSegment(Point(2, 1), Point(2, 6))
    assert(verticalSegment.contains(Point(2, 1)))
    assert(verticalSegment.contains(Point(2, 4)))
    assert(verticalSegment.contains(Point(2, 6)))
    assert(!verticalSegment.contains(Point(1, 1)))
    assert(!verticalSegment.contains(Point(2, 10)))
    assert(!verticalSegment.contains(Point(10, 12)))

    val horizontalSegment = LineSegment(Point(10, 2), Point(12, 2))
    assert(horizontalSegment.contains(Point(10, 2)))
    assert(horizontalSegment.contains(Point(12, 2)))
    assert(horizontalSegment.contains(Point(11, 2)))
    assert(!horizontalSegment.contains(Point(9, 1)))
    assert(!horizontalSegment.contains(Point(13, 2)))
    assert(!horizontalSegment.contains(Point(1, 2)))
  }

  test("Line contains") {
    val verticalLine = Line(Point(2, 1), Point(2, 6))
    assert(verticalLine.contains(Point(2, 1)))
    assert(verticalLine.contains(Point(2, 4)))
    assert(verticalLine.contains(Point(2, 6)))
    assert(!verticalLine.contains(Point(1, 1)))
    assert(verticalLine.contains(Point(2, 10)))
    assert(!verticalLine.contains(Point(10, 12)))

    val horizontalLine = Line(Point(10, 2), Point(12, 2))
    assert(horizontalLine.contains(Point(10, 2)))
    assert(horizontalLine.contains(Point(12, 2)))
    assert(horizontalLine.contains(Point(11, 2)))
    assert(!horizontalLine.contains(Point(9, 1)))
    assert(horizontalLine.contains(Point(13, 2)))
    assert(horizontalLine.contains(Point(1, 2)))
  }

  test("FiniteRockTrace contains") {
    val rockTrace = FiniteRockTrace(List(Point(2, 1), Point(2, 4), Point(7, 4)))
    assert(rockTrace.contains(Point(2, 1)))
    assert(rockTrace.contains(Point(2, 2)))
    assert(rockTrace.contains(Point(2, 4)))
    assert(rockTrace.contains(Point(5, 4)))
    assert(rockTrace.contains(Point(7, 4)))
    assert(!rockTrace.contains(Point(4, 2)))
    assert(!rockTrace.contains(Point(1, 5)))
  }

  test("FiniteRockTrace isBelow") {
    val rockTrace = FiniteRockTrace(List(Point(2, 1), Point(2, 4), Point(7, 4)))
    assert(rockTrace.isBelow(Point(3, 5)))
    assert(rockTrace.isBelow(Point(1, 10)))
    assert(!rockTrace.isBelow(Point(3, 4)))
    assert(!rockTrace.isBelow(Point(3, 2)))
  }

  test("Floor contains") {
    val floor = Floor(5)
    assert(floor.contains(Point(1, 5)))
    assert(floor.contains(Point(3, 5)))
    assert(!floor.contains(Point(1, 4)))
  }

  test("Floor isBelow") {
    val floor = Floor(5)
    assert(!floor.isBelow(Point(1, 5)))
    assert(!floor.isBelow(Point(3, 2)))
    assert(floor.isBelow(Point(1, 6)))
  }

  test("SimulationState isBlocked") {
    val rockTraces = List(
      FiniteRockTrace(List(Point(2, 1), Point(2, 4), Point(7, 4))),
      FiniteRockTrace(List(Point(3, 2), Point(5, 2)))
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
    val state = SimulationState(List(FiniteRockTrace(List(Point(0, 10), Point(5, 10)))))
    assertEquals(
      dropSand(state, Point(2, 0)),
      state.copy(stoppedSand = List(Point(2, 9)))
    )
  }

  test("dropSand: sand drops infinitely to the bottom") {
    val state = SimulationState(List(FiniteRockTrace(List(Point(0, 10), Point(5, 10)))))
    assertEquals(
      dropSand(state, Point(6, 0)),
      state.copy(freeFallingSand = true)
    )
  }

  test("dropSand: examples similar to examples from the task") {
    val rockTraces = List(
      FiniteRockTrace(List(Point(498, 4), Point(498, 6), Point(496, 6))),
      FiniteRockTrace(List(Point(503, 4), Point(502, 4), Point(502, 9), Point(494, 9)))
    )
    val state = SimulationState(rockTraces)
    val stateAfterDrop1 = dropSand(state, Point(500, 0))
    assertEquals(stateAfterDrop1, state.copy(stoppedSand = List(Point(500, 8))))

    val stateAfterDrop2 = dropSand(stateAfterDrop1, Point(500, 0))
    assertEquals(stateAfterDrop2, state.copy(stoppedSand = List(Point(500, 8), Point(499, 8))))

    val stateAfterDrop5 = (1 to 3).foldLeft(stateAfterDrop2)((state, _) => dropSand(state, Point(500, 0)))
    assertEquals(stateAfterDrop5, state.copy(
      stoppedSand = List(Point(500, 8), Point(499, 8), Point(501, 8), Point(500, 7), Point(498, 8))
    ))
  }
