package aoc2022.solutions

import Day4._

class Day4Spec extends munit.FunSuite:

  test("section intersection") {
    val section = Section(3, 7)
    assertEquals(section.intersect(Section(5, 9)), Some(Section(5, 7)))
    assertEquals(section.intersect(Section(8, 12)), None)
    assertEquals(section.intersect(section), Some(section))
  }

  test("one section fully contains another") {
    val section = Section(5, 12)
    assert(Section.oneFullyContainsAnother(section, Section(8, 10)))
    assert(!Section.oneFullyContainsAnother(section, Section(1, 4)))
  }
