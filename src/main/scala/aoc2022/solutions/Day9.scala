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
    override def toString: String =
      val sortedTrail = trail.toList.sortWith((a, b) =>
        if (a.y == b.y)
          a.x < b.x
        else
          a.y < b.y
      )
      s"TailTrail[ $sortedTrail ]"

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

  trait Rope(val head: Point):
    def withEmptyTrail: RopeAndTailTrail =
      RopeAndTailTrail(this, TailTrail.empty)
    def moveHead(headMovementVector: Point): Rope
    def dragTail: (Rope, TailTrail)

  case class RopeAndTailTrail(rope: Rope, tailTrail: TailTrail)

  case class TwoLinkRope(override val head: Point, tail: Point) extends Rope(head):

    def avoidMovingOntoHead(tailMovementVector: Point): Point =
      val coordinates = Set(tailMovementVector.x, tailMovementVector.y)
      if (coordinates.forall(Math.abs(_) <= 1))
        Point(0, 0)
      else
        tailMovementVector

    override def moveHead(headMovementVector: Point): Rope =
      this.copy(head = head.add(headMovementVector))

    override def dragTail: (TwoLinkRope, TailTrail) =
      val tailMovementVector = avoidMovingOntoHead(head.subtract(tail)).normalizeMovement
      val newRope = this.copy(tail = tail.add(tailMovementVector))
      val trail = TailTrail(Set(tail, newRope.tail))
      (newRope, trail)

  def move(ropeToMove: Rope, headMove: Move): RopeAndTailTrail =
    val movementVector = directionVector(headMove.direction)
    (1 to headMove.distance).foldLeft(ropeToMove.withEmptyTrail)({
      case (RopeAndTailTrail(rope: Rope, trailSoFar), _) =>
        val (updatedRope, trailPart) = rope.moveHead(movementVector).dragTail
        RopeAndTailTrail(updatedRope, trailSoFar.append(trailPart))
    })

  def applyMovesToRope(initialRope: RopeAndTailTrail, moves: Seq[Move]): RopeAndTailTrail =
    moves.foldLeft(initialRope)((ropeAndTrail, currentMove) =>
      val RopeAndTailTrail(rope, trailSoFar) = ropeAndTrail
      val RopeAndTailTrail(updatedRope, trailPart) = move(rope, currentMove)
      RopeAndTailTrail(updatedRope, trailSoFar.append(trailPart))
    )

  def solutionPart1(moves: Seq[Move]): Int =
    val initialRope = TwoLinkRope(Point(0, 0), Point(0, 0))
    val RopeAndTailTrail(_, finalTrail) = applyMovesToRope(initialRope.withEmptyTrail, moves)
    println(finalTrail)
    finalTrail.trail.size

@main def day9Main: Unit =
  import Day9._
  import Day9Input._
  val parsed = parse(input)
  // println(parsed)
  println(solutionPart1(parsed))
