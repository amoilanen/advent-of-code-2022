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

  test("LongRope.dragTail: when tail does not move") {
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

  test("LongRope.dragTail: when tail moves") {
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

  test("LongRope: apply move when tail also moves") {
    val rope = LongRope(Point(-3, 5), List(Point(-3, 6), Point(-2, 7), Point(-1, 7), Point(0, 7), Point(1, 7), Point(1, 6), Point(1, 5), Point(1, 4), Point(1, 3)))
    val headMove = Move(Direction.Right, 17)
    val RopeAndTailTrail(movedRope, trail) = move(rope, headMove)
    assertEquals(
      movedRope,
      LongRope(Point(14, 5), List(Point(13, 5), Point(12, 5), Point(11, 5), Point(10, 5), Point(9, 5), Point(8, 5), Point(7, 5), Point(6, 5), Point(5, 5)))
    )
    assert(trail.trail.contains(Point(1, 3)))
    assert(trail.trail.contains(Point(5, 5)))
  }