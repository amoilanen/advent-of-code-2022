package aoc2022.solutions

import scala.util.Try

object Day2:

  enum Shape:
    case Rock
    case Paper
    case Scissors
    def winsOver(other: Shape): Boolean =
      this match {
        case Rock => other == Scissors
        case Paper => other == Rock
        case Scissors => other == Paper
      }
    def score: Int =
      this.ordinal + 1

  object Shape:
    def fromCode(code: String): Option[Shape] =
      code match {
        case "A" | "X" => Some(Rock)
        case "B" | "Y" => Some(Paper)
        case "C" | "Z" => Some(Scissors)
        case _ => None
      }

  enum Outcome:
    case Lost
    case Draw
    case Won
    def score: Int =
      this match {
        case Lost => 0
        case Draw => 3
        case Won => 6
      }
  object Outcome:
    def fromCode(code: String): Option[Outcome] =
      code match {
        case "X" => Some(Lost)
        case "Y" => Some(Draw)
        case "Z" => Some(Won)
        case _ => None
      }

  case class Round(first: Shape, second: Shape)

  def parse(input: String): Seq[(String, String)] =
    input.split("\n").filter(_.nonEmpty).map(part =>
      val Array(first: String, second: String) = part.split(" ")
      (first, second)
    ).toSeq

  def computeOutcome(round: Round): Outcome =
    if (round.first == round.second)
      Outcome.Draw
    else if (round.first.winsOver(round.second))
      Outcome.Lost
    else
      Outcome.Won

  def computeScore(round: Round): Int =
    val outcome = computeOutcome(round)
    round.second.score + outcome.score

  def roundsForPart1(pairsOfCodes: Seq[(String, String)]): Seq[Round] =
    pairsOfCodes.map({ case (first, second) =>
      val round = for
        firstShape <- Shape.fromCode(first)
        secondShape <- Shape.fromCode(second)
      yield
        Round(firstShape, secondShape)
      if (round.isEmpty)
        println(s"'$first $second' - could not determine the round it encodes")
      round
    }).flatten

  def solution(roundDeterminator: Seq[(String, String)] => Seq[Round])(pairsOfCodes: Seq[(String, String)]): Int =
    val rounds = roundDeterminator(pairsOfCodes)
    rounds.map(computeScore(_)).sum

  val solutionPart1: Seq[(String, String)] => Int =
    solution(roundsForPart1)

  def determineShapeByOutcome(first: Shape, desiredOutcome: Outcome): Option[Shape] =
    if (desiredOutcome == Outcome.Draw)
      Some(first)
    else if (desiredOutcome == Outcome.Won)
      Shape.values.find(_.winsOver(first))
    else
      Shape.values.find(first.winsOver(_))

  def roundsForPart2(pairsOfCodes: Seq[(String, String)]): Seq[Round] =
    pairsOfCodes.map({ case (first, second) =>
      val round = for
        firstShape <- Shape.fromCode(first)
        desiredOutcome <- Outcome.fromCode(second)
        secondShape <- determineShapeByOutcome(firstShape, desiredOutcome)
      yield
        Round(firstShape, secondShape)
      if (round.isEmpty)
        println(s"'$first $second' - could not determine the round it encodes")
      round
    }).flatten

  val solutionPart2: Seq[(String, String)] => Int =
    solution(roundsForPart2)

@main
def day2Main: Unit =
  import Day2._
  import Day2Input._
  val parsed = parse(input)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))