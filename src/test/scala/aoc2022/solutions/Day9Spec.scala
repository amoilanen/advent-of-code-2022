package aoc2022.solutions

import Day9._

class Day9Spec extends munit.FunSuite:
  test("TwoLinkRope.dragTail: when trail moves") {
    assertEquals(
      TwoLinkRope(Point(3, 1), Point(1, 1)).dragTail,
      (TwoLinkRope(Point(3, 1), Point(2, 1)), TailTrail(Set(Point(1, 1), Point(2, 1))))
    )
    assertEquals(
      TwoLinkRope(Point(1, 1), Point(1, 3)).dragTail,
      (TwoLinkRope(Point(1, 1), Point(1, 2)), TailTrail(Set(Point(1, 3), Point(1, 2))))
    )
    assertEquals(
      TwoLinkRope(Point(2, 3), Point(1, 1)).dragTail,
      (TwoLinkRope(Point(2, 3), Point(2, 2)), TailTrail(Set(Point(1, 1), Point(2, 2))))
    )
    assertEquals(
      TwoLinkRope(Point(3, 2), Point(1, 1)).dragTail,
      (TwoLinkRope(Point(3, 2), Point(2, 2)), TailTrail(Set(Point(1, 1), Point(2, 2))))
    )
  }

  test("TwoLinkRope.dragTail: when trail does not move") {
    assertEquals(
      TwoLinkRope(Point(1, 0), Point(0, 0)).dragTail,
      (TwoLinkRope(Point(1, 0), Point(0, 0)), TailTrail(Set(Point(0, 0))))
    )
    assertEquals(
      TwoLinkRope(Point(2, 2), Point(1, 1)).dragTail,
      (TwoLinkRope(Point(2, 2), Point(1, 1)), TailTrail(Set(Point(1, 1))))
    )
  }

  test("LongRope.dragTail: when trail moves") {
    assertEquals(
      LongRope(Point(4, 0), List(Point(2, 0), Point(1, 0), Point(0, 0), Point(0, 0),
        Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0))).dragTail,
      (LongRope(Point(4, 0), List(Point(3, 0), Point(2, 0), Point(1, 0), Point(0, 0),
        Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0))), TailTrail(Set(Point(0, 0))))
    )
    assertEquals(
      LongRope(Point(4, 4), List(Point(4, 2), Point(3, 1), Point(2, 1), Point(1, 1), Point(0, 0), Point(0, 0),
        Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0))).dragTail,
      (LongRope(Point(4, 4), List(Point(4, 3), Point(4, 2), Point(3, 2), Point(2, 2), Point(1, 1), Point(0, 0),
        Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0))), TailTrail(Set(Point(0, 0))))
    )
  }
