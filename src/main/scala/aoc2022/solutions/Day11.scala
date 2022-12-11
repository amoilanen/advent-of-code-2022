package aoc2022.solutions

import aoc2022.solutions.common.ParsingUtils._

object Day11:

  case class Monkey(
    id: Int,
    initialItems: List[Int],
    operation: Int => Int,
    divisibleBy: Int,
    monkeyIdIfDivisible: Int,
    monkeyIdIfNotDivisible: Int
  )

  def parse(input: String): Seq[Monkey] =
    val splitInput = splitBy("")(input.split("\n").map(_.trim)).filter(_.size > 0)
    splitInput.map(parseMonkey)

  def parseMonkey(input: Seq[String]): Monkey =
    val Seq(idInput, itemsInput, operationInput, testInput, ifTrueInput, ifFalseInput) = input
    val id = idInput match {
      case s"Monkey $id:" =>
        id.toInt
    }
    val initialItems = itemsInput match {
      case s"Starting items: $items" =>
        items.split(",").map(_.trim.toInt).toList
    }
    val operation = operationInput match {
      case s"Operation: $operation" =>
        parseMonkeyFunction(operation)
    }
    val divisibleBy = testInput match {
      case s"Test: divisible by $value" =>
        value.toInt
    }
    val monkeyIdIfDivisible = ifTrueInput match {
      case s"If true: throw to monkey $id" =>
        id.toInt
    }
    val monkeyIdIfNotDivisible = ifFalseInput match {
      case s"If false: throw to monkey $id" =>
        id.toInt
    }
    Monkey(id, initialItems, operation, divisibleBy, monkeyIdIfDivisible, monkeyIdIfNotDivisible)

  def parseMonkeyFunction(input: String): Int => Int =
    input match {
      case s"new = old + old" => (x: Int) => x + x
      case s"new = old * old" => (x: Int) => x * x
      case s"new = old + $value" => (x: Int) => x + value.toInt
      case s"new = old * $value" => (x: Int) => x * value.toInt
      case s"new = $value + old" => (x: Int) => x + value.toInt
      case s"new = $value * old" => (x: Int) => x * value.toInt
    }

@main
def day11Main: Unit =
  import Day11._
  import Day11Input._
  val parsed = parse(input)
  println(parsed)
