package aoc2022.solutions

import aoc2022.solutions.common.ParsingUtils.{ParsingError, splitBy}

object Day4:

  case class Section(start: Int, end: Int):
    def intersect(other: Section): Option[Section] =
      val newStart = Math.max(start, other.start)
      val newEnd = Math.min(end, other.end)
      if (newStart <= newEnd)
        Some(Section(newStart, newEnd))
      else
        None
    def contains(other: Section): Boolean =
      this.intersect(other) == Some(other)

  object Section:
    def oneFullyContainsAnother(first: Section, second: Section): Boolean =
      val intersection = first.intersect(second)
      intersection == Some(first) || intersection == Some(second)

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

  def solutionPart1(pairs: Seq[ElfPair]): Int =
    pairs.filter(pair =>
      Section.oneFullyContainsAnother(pair.first, pair.second)
    ).size

  def solutionPart2(pairs: Seq[ElfPair]): Int =
    pairs.filter(pair =>
      !pair.first.intersect(pair.second).isEmpty
    ).size

@main def day4Solution: Unit =
  import Day4._
  import Day4Input._
  val parsed = parse(input)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))
