package aoc2022.solutions

object Day3:

  case class Rucksack(contents: String):
    private val totalItems = contents.size
    val firstCompartment: String = contents.substring(0, totalItems / 2)
    val secondCompartment: String = contents.substring(totalItems / 2)

  def parse(input: String): Seq[Rucksack] =
    input.split("\n").filter(_.nonEmpty).map(Rucksack(_))

  def groupsOfRucksacks(rucksacks: Seq[Rucksack]): Seq[Seq[Rucksack]] =
    rucksacks.grouped(3).toSeq

  def scoreItem(item: Char): Int =
    if (item.isLower)
      item.toInt - 'a'.toInt + 1
    else
      item.toInt - 'A'.toInt + 27

  def commonItem(itemCollections: Seq[String]): Char =
    val itemSets = itemCollections.map(_.toSet)
    val intersection = itemSets.reduce((x, y) => x.intersect(y))
    intersection.toList.head

  def solutionPart1(rucksacks: Seq[Rucksack]): Int =
    rucksacks.map(rucksack =>
      commonItem(Seq(
        rucksack.firstCompartment,
        rucksack.secondCompartment
      )),
    ).map(scoreItem(_)).sum

  def solutionPart2(rucksacks: Seq[Rucksack]): Int =
    groupsOfRucksacks(rucksacks).map(rucksackGroup =>
      commonItem(rucksackGroup.map(_.contents))
    ).map(scoreItem(_)).sum

@main
def day3Main: Unit =
  import Day3._
  import Day3Input._
  val parsed = parse(input)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))