package aoc2022.solutions

import scala.annotation.tailrec

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
        case _ =>
          false
      }
  case class ListOf[A: Ordering](elements: List[Expression[A]]) extends Expression[A]
  case class Element[A: Ordering](value: A) extends Expression[A]

  object Expression:
    def l[A: Ordering](elements: Expression[A]*): Expression[A] =
      ListOf(elements.toList)
    def e[A: Ordering](element: A): Expression[A] =
      Element[A](element)

  type Packet = Expression[Int]
  case class PacketPair(index: Int, left: Packet, right: Packet)

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
    ???

@main
def day13Main: Unit =
  import Day13._
  import Day13Input._
  val parsed = parse(input)
  println(parsed)

