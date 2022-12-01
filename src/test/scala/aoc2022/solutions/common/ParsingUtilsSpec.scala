package aoc2022.solutions.common

import ParsingUtils._

class ParsingUtilsSpec extends munit.FunSuite:

  test("splitBy: delimiter occurs several times") {
    assertEquals(
      splitBy('a')("abcabcdaaab".toList).map(_.mkString),
      Seq("", "bc", "bcd", "", "", "b")
    )
  }

  test("splitBy: delimiter does not occur") {
    assertEquals(
      splitBy('d')("abcaba".toList).map(_.mkString),
      Seq("abcaba")
    )
  }

  test("splitBy: delimiter occurs ones") {
    assertEquals(
      splitBy('c')("abcde".toList).map(_.mkString),
      Seq("ab", "de")
    )
  }

  test("splitBy: split by empty string (used often when parsing)") {
    val input = Seq("", "a", "b", "", "c")
    val actual = splitBy("")(input)
    val expected = Seq(Seq(), Seq("a", "b"), Seq("c"))
    assertEquals(actual, expected)
  }