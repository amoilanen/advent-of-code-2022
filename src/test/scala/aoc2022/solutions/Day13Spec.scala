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

  /*
  test("comparing packets: acceptance test from the task description") {
    assert(parsePacket("[1,1,3,1,1]").lowerThan(parsePacket("[1,1,5,1,1]")))
    assert(parsePacket("[[1],[2,3,4]]").lowerThan(parsePacket("[[1],4]")))
    assert(!parsePacket("[9]").lowerThan(parsePacket("[[8,7,6]]")))
    assert(parsePacket("[[4,4],4,4]").lowerThan(parsePacket("[[4,4],4,4,4]")))
    assert(!parsePacket("[7,7,7,7]").lowerThan(parsePacket("[7,7,7]")))
    assert(parsePacket("[]").lowerThan(parsePacket("[3]")))
    assert(!parsePacket("[[[]]]").lowerThan(parsePacket("[[]]")))
    assert(!parsePacket("[1,[2,[3,[4,[5,6,7]]]],8,9]").lowerThan(parsePacket("[1,[2,[3,[4,[5,6,0]]]],8,9]")))
  }
  */

  test("comparing packets") {
    assert(e(3).lowerThan(e(5)))
    assert(!e(3).lowerThan(e(3)))
    assert(!e(4).lowerThan(e(2)))
    assert(l(e(3)).lowerThan(l(e(5))))
    assert(!l(e(3)).lowerThan(l(e(3))))
    assert(!l(e(4)).lowerThan(l(e(2))))
    assert(l(e(2), e(3), e(5)).lowerThan(l(e(2), e(3), e(7))))
    assert(!l(e(2), e(3), e(7)).lowerThan(l(e(2), e(3), e(5))))
    assert(l(e(2), e(3)).lowerThan(l(e(2), e(3), e(7))))
    assert(!l(e(2), e(3), e(7)).lowerThan(l(e(2), e(3))))
    assert(l(l(e(1), e(2)), l(e(3), e(4))).lowerThan(l(l(e(1), e(2)), l(e(3), e(6)))))
  }
