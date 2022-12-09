package aoc2022.solutions

object Day9:

  enum Direction:
    case Right, Up, Left, Down

  object Direction:
    def parse(input: String): Direction = input match {
      case "R" => Right
      case "U" => Up
      case "L" => Left
      case "D" => Down
    }

  case class Move(direction: Direction, distance: Int)

  def parseMove(moveInput: String): Move =
    moveInput.split(" ") match {
      case Array(direction, distance) =>
        Move (Direction.parse(direction), distance.toInt)
    }

  def parse(input: String): Seq[Move] =
    input.split("\n").map(_.trim).filter(_.nonEmpty).map(parseMove)

@main def day9Main: Unit =
  import Day9._
  import Day9Input._
  val parsed = parse(input)
  println(parsed)
