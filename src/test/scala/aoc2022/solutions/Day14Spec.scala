package aoc2022.solutions

import aoc2022.solutions.Day14.{Point, RockTrace, SimulationState, dropSand}

class Day14Spec extends munit.FunSuite:

  //TODO: Tests for Line contains
  //TODO: Tests for RockTrace contains
  //TODO: Tests for SimulationState contains

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
