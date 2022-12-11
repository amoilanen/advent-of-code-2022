package aoc2022.solutions

object Day11:

  case class Monkey(
    id: Int,
    initialItems: List[Int],
    operation: Int => Int,
    divisibleBy: Int,
    monkeyIdIfDivisible: Int,
    monkeyIdIfNotDivisible: Int
  )

  def parse(input: String): List[Monkey] =
    ???

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
