package aoc2022.solutions

import scala.annotation.tailrec
import scala.util.Try

import aoc2022.solutions.common.ParsingUtils._

object Day1:

  case class ElfItems(items: Seq[Int])

  def parse(input: String): Seq[ElfItems] =
    val lines = input.split("\n").map(_.trim)
    val itemsGroupedByElf = splitBy("")(lines).filter(!_.isEmpty).map(_.map(_.toInt))
    itemsGroupedByElf.map(ElfItems(_))

  def elvesCaloriesSortedByHighest(elves: Seq[ElfItems]): Seq[Int] =
    elves.map(_.items.sum).sorted.reverse

  def solutionPart1(elves: Seq[ElfItems]): Int =
    elvesCaloriesSortedByHighest(elves).take(1).sum

  def solutionPart2(elves: Seq[ElfItems]): Int =
    elvesCaloriesSortedByHighest(elves).take(3).sum

@main def dayOneSolution: Unit =
  import Day1._
  import Day1Input._
  val parsed = parse(input)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))
