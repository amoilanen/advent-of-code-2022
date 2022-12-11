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
  ):
    def inspectItem(item: Int, worryLevelReducer: Int): (Int, Int) =
      val worryLevel = operation(item) / worryLevelReducer
      val nextMonkeyId = if (worryLevel % divisibleBy == 0)
        monkeyIdIfDivisible
      else
        monkeyIdIfNotDivisible
      (worryLevel, nextMonkeyId)

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

  case class MutableRoundState(monkeyItems: Array[List[Int]], monkeyOperations: Array[Int])
  object MutableRoundState:
    def initial(monkeys: Seq[Monkey]): MutableRoundState =
      val items = monkeys.map(_.initialItems).toArray
      val operations = (0 until monkeys.size).map(_ => 0).toArray
      MutableRoundState(items, operations)

  def evaluateRound(monkeys: Seq[Monkey], worryLevelReducer: Int, roundState: MutableRoundState): MutableRoundState =
    monkeys.foreach(monkey =>
      val monkeyId = monkey.id
      val monkeyItems = roundState.monkeyItems(monkeyId)
      val itemsWithNextMonkeys = monkeyItems.map(monkey.inspectItem(_, worryLevelReducer))
      itemsWithNextMonkeys.foreach(itemWithMonkeyId =>
        val (item, nextMonkeyId) = itemWithMonkeyId
        roundState.monkeyOperations(monkeyId) = roundState.monkeyOperations(monkeyId) + 1
        roundState.monkeyItems(nextMonkeyId) = roundState.monkeyItems(nextMonkeyId) :+ item
      )
      roundState.monkeyItems(monkeyId) = List()
    )
    roundState

  def evaluateRounds(monkeys: Seq[Monkey], worryLevelReducer: Int, roundsNumber: Int): MutableRoundState =
    (1 to roundsNumber).foldLeft(MutableRoundState.initial(monkeys))((state, _) =>
      evaluateRound(monkeys, worryLevelReducer, state)
    )

  def operationCounts(monkeys: Seq[Monkey], worryLevelReducer: Int, roundsNumber: Int): List[Int] =
    val MutableRoundState(_, monkeyOperations) = evaluateRounds(monkeys, worryLevelReducer, roundsNumber)
    monkeyOperations.toList

  def levelOfMonkeyBusiness(monkeys: Seq[Monkey], worryLevelReducer: Int, roundsNumber: Int) =
    operationCounts(monkeys, worryLevelReducer, roundsNumber).sorted.reverse.take(2).reduce(_ * _)

  val Part1WorryLevelReducer = 3
  val Part1Rounds = 20

  def solutionPart1(monkeys: Seq[Monkey]): Int =
    levelOfMonkeyBusiness(monkeys, Part1WorryLevelReducer, Part1Rounds)

@main
def day11Main: Unit =
  import Day11._
  import Day11Input._
  val parsed = parse(input)
  println(solutionPart1(parsed))
