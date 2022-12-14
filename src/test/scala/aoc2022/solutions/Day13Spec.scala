package aoc2022.solutions

import Day13._

class Day13Spec extends munit.FunSuite:
  test("parsePacket") {
    /*assertEquals(
      parsePacket("[1]"),
      List(1)
    )*/
    assertEquals(
      parsePacket("[1,1,3,1,1]"),
      List(1, 1, 3, 1, 1)
    )
    assertEquals(
      parsePacket("[[1],[2,3,4]]"),
      List(List(1), List(2, 3, 4))
    )

    assertEquals(
      parsePacket("[[4,4],4,4]"),
      List(List(4, 4), 4, 4)
    )
    assertEquals(
      parsePacket("[[[]]]"),
      List(List(List()))
    )
    assertEquals(
      parsePacket("[4,1,[2,[6]],10,3,[4,5]]"),
      List(4, 1, List(2, List(6)), 10, 3, List(4, 5))
    )
  }
