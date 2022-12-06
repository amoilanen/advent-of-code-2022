package aoc2022.solutions

import Day6._

class Day6Spec extends munit.FunSuite:
  test("findMessageStart") {
    assertEquals(findMessageStart("bvwbjplbgvbhsrlpgdmjqwftvncz"), Some(5))
    assertEquals(findMessageStart("nppdvjthqldpwncqszvftbrmjlhg"), Some(6))
    assertEquals(findMessageStart("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), Some(10))
    assertEquals(findMessageStart("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), Some(11))
  }
