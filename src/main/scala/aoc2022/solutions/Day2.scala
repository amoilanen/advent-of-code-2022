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

  case class Round(first: Shape, second: Shape)

  def parse(input: String): Seq[Round] =
    input.split("\n").filter(_.nonEmpty).map(part =>
      val Array(firstShape: String, secondShape: String) = part.split(" ")
      for
        first <- Shape.fromCode(firstShape)
        second <- Shape.fromCode(secondShape)
      yield
        Round(first, second)
    ).toSeq.flatten

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

  def solutionPart1(rounds: Seq[Round]): Int =
    rounds.map(computeScore(_)).sum

@main
def day2Main: Unit =
  import Day2._
  import Day2Input._
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))