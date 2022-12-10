package aoc2022.solutions

object Day10:
  trait Instruction
  case class Add(value: Int) extends Instruction
  object Noop extends Instruction:
    override def toString: String = "Noop"

  def parse(input: String): Seq[Instruction] =
    input.split("\n").map(_.trim).filter(_.nonEmpty).map({
      case "noop" => Noop
      case s"addx $value" => Add(value.toInt)
    })

@main
def day10Main: Unit =
  import Day10._
  import Day10Input._
  val parsed = parse(input)
  println(parsed)
