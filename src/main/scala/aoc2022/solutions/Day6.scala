package aoc2022.solutions

object Day6:
  def parse(input: String): String =
    input

  def nCharacterGroups(input: String, n: Int): LazyList[(String, Int)] =
    LazyList.from(0 until input.size).map(index =>
      (input.substring(index, index + n), index)
    )

  def findGroupOfUniqueCharacters(characterGroups: LazyList[(String, Int)]): Option[(String, Int)] =
    characterGroups.find({
      case (group, _) =>
        group.toSeq.toSet.size == group.size
    })

  def findMessageStart(puzzleInput: String, groupSize: Int): Option[Int] =
    findGroupOfUniqueCharacters(nCharacterGroups(puzzleInput, groupSize)).map(_._2 + groupSize)

  def solutionPart1(puzzleInput: String): Int =
    findMessageStart(puzzleInput, 4).getOrElse(-1)

  def solutionPart2(puzzleInput: String): Int =
    findMessageStart(puzzleInput, 14).getOrElse(-1)

@main def day6Main: Unit =
  import Day6._
  import Day6Input._
  val parsed = parse(input)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))
