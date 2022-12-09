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

  // We can view Point both a point on a 2D plain but also as a Vector on a 2D plain
  case class Point(x: Int, y: Int):
    def subtract(other: Point): Point =
      Point(x - other.x, y - other.y)
    def add(other: Point): Point =
      Point(x + other.x, y + other.y)
    private def normalizeMovement(coordinate: Int) =
      if (coordinate == 0)
        0
      else
        coordinate / Math.abs(coordinate)
    def normalizeMovement: Point =
      Point(normalizeMovement(x), normalizeMovement(y))

  case class TailTrail(trail: Set[Point])

  def directionVector(direction: Direction): Point =
    import Direction._
    direction match {
      case Right => Point(1, 0)
      case Up => Point(0, 1)
      case Left => Point(-1, 0)
      case Down => Point(0, -1)
    }

  case class Rope(head: Point, tail: Point):

    def avoidMovingOntoHead(tailMovementVector: Point): Point =
      val coordinates = Set(tailMovementVector.x, tailMovementVector.y)
      if (coordinates.forall(Math.abs(_) <= 1))
        Point(0, 0)
      else
        tailMovementVector

    def dragTail: (Rope, TailTrail) =
      val tailMovementVector = avoidMovingOntoHead(head.subtract(tail)).normalizeMovement
      val newRope = this.copy(tail = tail.add(tailMovementVector))
      val trail = TailTrail(Set(tail, newRope.tail))
      (newRope, trail)

    def move(headMove: Move): Rope =
      ???

@main def day9Main: Unit =
  import Day9._
  import Day9Input._
  val parsed = parse(input)
  println(parsed)
