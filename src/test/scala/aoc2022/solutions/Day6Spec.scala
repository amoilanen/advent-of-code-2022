package aoc2022.solutions

import Day6._

class Day6Spec extends munit.FunSuite:
  test("findMessageStart: groups of 4 symbols") {
    val groupSize = 4
    assertEquals(findMessageStart("bvwbjplbgvbhsrlpgdmjqwftvncz", groupSize), Some(5))
    assertEquals(findMessageStart("nppdvjthqldpwncqszvftbrmjlhg", groupSize), Some(6))
    assertEquals(findMessageStart("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", groupSize), Some(10))
    assertEquals(findMessageStart("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", groupSize), Some(11))
  }

  test("findMessageStart: groups of 14 symbols") {
    val groupSize = 14
    assertEquals(findMessageStart("mjqjpqmgbljsphdztnvjfqwrcgsmlb", groupSize), Some(19))
    assertEquals(findMessageStart("bvwbjplbgvbhsrlpgdmjqwftvncz", groupSize), Some(23))
    assertEquals(findMessageStart("nppdvjthqldpwncqszvftbrmjlhg", groupSize), Some(23))
    assertEquals(findMessageStart("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", groupSize), Some(29))
    assertEquals(findMessageStart("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", groupSize), Some(26))
  }
