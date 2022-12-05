package aoc2022.solutions

import aoc2022.solutions.common.ParsingUtils._

object Day5:

  case class StacksOfCrates(crates: List[List[String]])
  case class Operation(quantity: Int, fromIndex: Int, toIndex: Int)
  case class PuzzleInput(stacks: StacksOfCrates, operations: List[Operation])

  def parseCrossStackCut(input: String): List[Option[String]] =
    input.grouped(4).toList.map(_.trim).map({
      case s"[$character]" => Some(character)
      case _ => None
    })

  def parseStacksOfCrates(cratesInput: Seq[String]): StacksOfCrates =
    val indexes = cratesInput.last.split("\\s+").filter(_.nonEmpty).map(_.toInt)
    val maxIndex = indexes.max
    val stackCrossCuts = cratesInput.dropRight(1).map(parseCrossStackCut)
    val stacks = (0 until maxIndex).map(index =>
      stackCrossCuts.map(crossCut =>
        if (index >= crossCut.size)
          None
        else
          crossCut(index)
      ).dropWhile(_.isEmpty).reverse.flatten.toList.reverse
    ).toList
    StacksOfCrates(stacks)

  def parseOperation(operationInput: String): Operation =
    operationInput match {
      case s"move $quantity from $from to $to" =>
        Operation(quantity.toInt, from.toInt, to.toInt)
    }

  def parseOperations(operationsInput: List[String]): List[Operation] =
    operationsInput.map(parseOperation)

  def parse(input: String): PuzzleInput =
    val lines = input.split("\n").dropWhile(_.trim.isEmpty).toList
    val inputSeparatorIndex = lines.map(_.trim).indexOf("")
    val cratesInput = lines.slice(0, inputSeparatorIndex)
    val operationsInput = lines.slice(inputSeparatorIndex + 1, lines.length)
    val stacks = parseStacksOfCrates(cratesInput)
    val operations = parseOperations(operationsInput)
    PuzzleInput(stacks, operations)

@main def day5Solution: Unit =
  import Day5._
  import Day5Input._
  val parsed = parse(input)
  println(parsed)
