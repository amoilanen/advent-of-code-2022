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

  case class TailTrail(trail: Set[Point]):
    def append(other: TailTrail): TailTrail =
      TailTrail(trail.union(other.trail))

  object TailTrail:
    val empty: TailTrail =
      TailTrail(Set())

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

    def withEmptyTrail: RopeAndTailTrail =
      RopeAndTailTrail(this, TailTrail.empty)

    def move(headMove: Move): RopeAndTailTrail =
      val movementVector = directionVector(headMove.direction)
      (1 to headMove.distance).foldLeft(this.withEmptyTrail)({ (ropeAndTrail, _) =>
        val RopeAndTailTrail(rope, trailSoFar) = ropeAndTrail
        val (updatedRope, trailPart) = rope.copy(head = rope.head.add(movementVector)).dragTail
        RopeAndTailTrail(updatedRope, trailSoFar.append(trailPart))
      })

  case class RopeAndTailTrail(rope: Rope, tailTrail: TailTrail)

  def applyMoves(initialRope: Rope, moves: Seq[Move]): RopeAndTailTrail =
    moves.foldLeft(initialRope.withEmptyTrail)((ropeAndTrail, currentMove) =>
      val RopeAndTailTrail(rope, trailSoFar) = ropeAndTrail
      val RopeAndTailTrail(updatedRope, trailPart) = rope.move(currentMove)
      RopeAndTailTrail(updatedRope, trailSoFar.append(trailPart))
    )

  def solutionForPart1(moves: Seq[Move]): Int =
    val initialRope = Rope(Point(0, 0), Point(0, 0))
    val RopeAndTailTrail(_, finalTrail) = applyMoves(initialRope, moves)
    /*
    val sortedTrail = finalTrail.trail.toList.sortWith((a, b) =>
      if (a.y == b.y)
        a.x < b.x
      else
        a.y < b.y
    )
    println(sortedTrail)
    */
    finalTrail.trail.size

@main def day9Main: Unit =
  import Day9._
  import Day9Input._
  val parsed = parse(input)
  // println(parsed)
  println(solutionForPart1(parsed))
