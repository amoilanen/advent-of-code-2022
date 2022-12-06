package aoc2022.solutions

object Day6:
  def parse(input: String): String =
    input

  def fourCharacterGroups(input: String): LazyList[(String, Int)] =
    LazyList.from(0 until input.size).map(index =>
      (input.substring(index, index + 4), index)
    )

  def findFindGroupOfUniqueCharacters(characterGroups: LazyList[(String, Int)]): Option[(String, Int)] =
    characterGroups.find({
      case (group, _) =>
        group.toSeq.toSet.size == group.size
    })

  def findMessageStart(puzzleInput: String): Option[Int] =
    findFindGroupOfUniqueCharacters(fourCharacterGroups(puzzleInput)).map(_._2 + 4)

  def solutionForPart1(puzzleInput: String): Int =
    findMessageStart(puzzleInput).getOrElse(-1)

@main def day6Main: Unit =
  import Day6._
  import Day6Input._
  val parsed = parse(input)
  println(solutionForPart1(parsed))
