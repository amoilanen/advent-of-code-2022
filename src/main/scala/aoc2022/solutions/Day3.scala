package aoc2022.solutions

object Day3:

  case class Rucksack(contents: String):
    private val totalItems = contents.size
    val firstCompartment: String = contents.substring(0, totalItems / 2)
    val secondCompartment: String = contents.substring(totalItems / 2)

  def parse(input: String): Seq[Rucksack] =
    input.split("\n").filter(_.nonEmpty).map(Rucksack(_))

  def itemAppearingInBothCompartments(rucksack: Rucksack): Char =
    val wronglyPackedItem = rucksack.firstCompartment.find(rucksack.secondCompartment.contains(_)).get
    wronglyPackedItem

  def scoreItem(item: Char): Int =
    if (item.isLower)
      item.toInt - 'a'.toInt + 1
    else
      item.toInt - 'A'.toInt + 27

  def solutionPart1(rucksacks: Seq[Rucksack]): Int =
    rucksacks.map(itemAppearingInBothCompartments(_)).map(scoreItem(_)).sum

@main
def day3Main: Unit =
  import Day3._
  import Day3Input._
  val parsed = parse(input)
  println(solutionPart1(parsed))