package aoc2022.solutions

import scala.annotation.tailrec
import aoc2022.solutions.common.ParsingUtils._

object Day13:

  // Not possible to define a recursive type with the Scala type system:
  // type NestedList[A] = A | List[NestedList[A]]
  // https://github.com/lampepfl/dotty/issues/10136
  abstract class Expression[A: Ordering]:
    def lowerThan(other: Expression[A]): Boolean =
      val ordering = summon[Ordering[A]]
      (this, other) match {
        case (Element(x), Element(y)) =>
          ordering.lt(x, y)
        case (x: Element[A], y: ListOf[A]) =>
          ListOf(List(x)).lowerThan(y)
        case (x: ListOf[A], y: Element[A]) =>
          x.lowerThan(ListOf(List(y)))
        case (ListOf(xElements), ListOf(yElements)) =>
          (xElements, yElements) match {
            case (xHead::xTail, yHead::yTail) =>
              if (xHead.lowerThan(yHead))
                true
              else if (yHead.lowerThan(xHead))
                false
              else
                ListOf(xTail).lowerThan(ListOf(yTail))
            case (Nil, _::_) =>
              true
            case (_::_, Nil) =>
              false
            case (Nil, Nil) =>
              false
          }
        case _ =>
          false
      }

  case class ListOf[A: Ordering](elements: List[Expression[A]]) extends Expression[A]:
    override def toString: String =
      val elementsRepresentation = elements.map(_.toString).mkString(",")
      s"[$elementsRepresentation]"
  case class Element[A: Ordering](value: A) extends Expression[A]:
    override def toString: String =
      value.toString

  object Expression:
    def l[A: Ordering](elements: Expression[A]*): Expression[A] =
      ListOf(elements.toList)
    def e[A: Ordering](element: A): Expression[A] =
      Element[A](element)

  type Packet = Expression[Int]
  case class PacketPair(index: Int, left: Packet, right: Packet):
    val packets = List(left, right)

  @tailrec
  def parsePacket(input: List[String], position: Int, currentLists: List[ListOf[Int]], partialNumber: List[String]): Packet =
    def flushNumberToList(expression: ListOf[Int], number: List[String]): ListOf[Int] =
      val readyNumber = partialNumber.mkString
      if partialNumber.length > 0 then
        ListOf(expression.elements :+ Element(readyNumber.toInt))
      else
        expression

    if position == input.length then
      currentLists.head
    else
      input(position) match {
        case "[" =>
          parsePacket(input, position + 1, ListOf(List.empty) +: currentLists, List())
        case "]" =>
          val updatedTopList = flushNumberToList(currentLists.head, partialNumber)
          val updatedParentList = currentLists.tail.headOption.map(parentList =>
            ListOf(elements = parentList.elements :+ updatedTopList)
          ).getOrElse(updatedTopList)
          parsePacket(input, position + 1, updatedParentList +: currentLists.drop(2), List())
        case "," =>
          val updatedTopList = flushNumberToList(currentLists.head, partialNumber)
          parsePacket(input, position + 1, updatedTopList +: currentLists.drop(1), List())
        case digit: String =>
          parsePacket(input, position + 1, currentLists, partialNumber ++ List(digit))
      }

  def parsePacket(input: String): Packet =
    parsePacket(input.split("").filter(_.nonEmpty).toList, 0, List.empty, List())

  def parse(input: String): Seq[PacketPair] =
    val lines = input.split("\n").map(_.trim)
    val lineGroups = splitBy("")(lines).filter(_.nonEmpty)
    lineGroups.zipWithIndex.map({
      case (List(left: String, right: String), index) =>
        val leftPacket = parsePacket(left)
        val rightPacket = parsePacket(right)
        PacketPair(index + 1, leftPacket, rightPacket)
    })

  def solutionPart1(pairs: Seq[PacketPair]): Int =
    pairs.filter(pair => pair.left.lowerThan(pair.right)).map(_.index).sum

  val dividerPackets = List(parsePacket("[[2]]"), parsePacket("[[6]]"))

  def solutionPart2(pairs: Seq[PacketPair]): Int =
    val allPackets = pairs.flatMap(_.packets) ++ dividerPackets
    val sortedPackets = allPackets.sortWith((x, y) => x.lowerThan(y))
    val dividerPacketIndices = dividerPackets.map(sortedPackets.indexOf(_) + 1)
    dividerPacketIndices.reduce(_ * _)

@main
def day13Main: Unit =
  import Day13._
  import Day13Input._
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))

