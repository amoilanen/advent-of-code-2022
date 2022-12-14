package aoc2022.solutions

import Day13._

class Day13Spec extends munit.FunSuite:
  import Expression._
  test("parsePacket") {
    assertEquals(
      parsePacket("[1]"),
      l(e(1))
    )
    assertEquals(
      parsePacket("[1,1,3,1,1]"),
      l(e(1), e(1), e(3), e(1), e(1))
    )
    assertEquals(
      parsePacket("[[1],[2,3,4]]"),
      l(l(e(1)), l(e(2), e(3), e(4)))
    )

    assertEquals(
      parsePacket("[[4,4],4,4]"),
      l(l(e(4), e(4)), e(4), e(4))
    )
    assertEquals(
      parsePacket("[[[]]]"),
      l(l(l[Int]()))
    )
    assertEquals(
      parsePacket("[4,1,[2,[6]],10,3,[4,5]]"),
      l(e(4), e(1), l(e(2), l(e(6))), e(10), e(3), l(e(4), e(5)))
    )
  }
