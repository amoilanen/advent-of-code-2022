package aoc2022.solutions

import Day16._

class Day16Spec extends munit.FunSuite:
  val ValveIds = "ABCDE".toList.map(id => ValveId(id.toString))
  val Seq(a, b, c, d, e) = ValveIds
  test("findShortestPaths") {
    val valves = Seq(
      Valve(a, 0, Seq(b, d)),
      Valve(b, 0, Seq(a, c, e)),
      Valve(c, 0, Seq(b, e)),
      Valve(d, 0, Seq(a)),
      Valve(e, 0, Seq(b, c)),
    )
    val shortestPaths = findShortestPaths(valves)
    val expectedShortedPaths = List(
      (a, a) -> 0,
      (a, b) -> 1,
      (a, c) -> 2,
      (a, d) -> 1,
      (a, e) -> 2,
      (b, b) -> 0,
      (b, c) -> 1,
      (b, d) -> 2,
      (b, e) -> 1,
      (c, c) -> 0,
      (c, d) -> 3,
      (c, e) -> 1,
      (d, d) -> 0,
      (d, e) -> 3,
      (e, e) -> 0,
    ).flatMap({ case ((from, to), distance) =>
      List((from, to) -> distance, (to, from) -> distance)
    }).toMap
    assertEquals(shortestPaths, expectedShortedPaths)
  }
