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

  def cratePickingOperationPart1(stack: List[String], quantity: Int): List[String] =
    stack.slice(0, quantity).reverse

  def cratePickingOperationPart2(stack: List[String], quantity: Int): List[String] =
    stack.slice(0, quantity)

  def applyOperation(cratePickingOperation: (stack: List[String], quantity: Int) => List[String])(stacksOfCrates: StacksOfCrates, operation: Operation): StacksOfCrates =
    val Operation(quantity, from, to) = operation
    val toAppend = cratePickingOperation(stacksOfCrates.crates(from - 1), quantity)
    StacksOfCrates(stacksOfCrates.crates.zipWithIndex.map({ case (stack, idx) =>
      if (idx == from - 1)
        stack.drop(quantity)
      else if (idx == to - 1)
         toAppend ++ stack
      else
        stack
    }))

  def topCratesAfterOperations(puzzleInput: PuzzleInput, cratePickingOperation: (stack: List[String], quantity: Int) => List[String]): String =
    val PuzzleInput(stacks, operations) = puzzleInput
    val applyOperationWithPicking = applyOperation(cratePickingOperation)
    val stacksOfCratesAfterOperations = operations.foldLeft(stacks)((updatedStacks, op) => applyOperationWithPicking(updatedStacks, op))
    stacksOfCratesAfterOperations.crates.map(_.head).flatten.mkString

  def solutionPart1(puzzleInput: PuzzleInput): String =
    topCratesAfterOperations(puzzleInput, cratePickingOperationPart1)

  def solutionPart2(puzzleInput: PuzzleInput): String =
    topCratesAfterOperations(puzzleInput, cratePickingOperationPart2)

@main def day5Solution: Unit =
  import Day5._
  import Day5Input._
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))
