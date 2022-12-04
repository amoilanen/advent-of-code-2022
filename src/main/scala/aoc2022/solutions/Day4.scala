package aoc2022.solutions

import aoc2022.solutions.common.ParsingUtils.{ParsingError, splitBy}

object Day4:

  case class Section(start: Int, end: Int)
  case class ElfPair(first: Section, second: Section)

  def parseSection(input: String): Section =
    val ends = input.split('-')
    ends.map(_.toInt) match {
      case Array(start, end) =>
        Section(start, end)
      case _ =>
        throw ParsingError(s"Could not parse '$input' into two sections, invalid input?")
    }

  def parseElfPair(input: String): ElfPair =
    val sections = input.split(',')
    sections match {
      case Array(first, second) =>
        ElfPair(parseSection(first), parseSection(second))
      case _ =>
        throw ParsingError(s"Could not parse '$input' into two sections, invalid input?")
    }

  def parse(input: String): Seq[ElfPair] =
    val lines = input.split("\n").map(_.trim).filter(_.nonEmpty)
    lines.map(parseElfPair(_))

  def solutionPart1(elves: Seq[ElfPair]): Int =
    ???

  def solutionPart2(elves: Seq[ElfPair]): Int =
    ???

@main def day4Solution: Unit =
  import Day4._
  import Day4Input._
  val parsed = parse(input)
  println(parsed)
