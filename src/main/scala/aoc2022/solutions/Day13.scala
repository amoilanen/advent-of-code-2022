package aoc2022.solutions

import scala.annotation.tailrec

object Day13:

  // Not possible to define a recursive type with the Scala type system:
  // type NestedList[A] = A | List[NestedList[A]]
  // https://github.com/lampepfl/dotty/issues/10136
  trait Expression[A]
  case class ListOf[A](elements: List[Expression[A]]) extends Expression[A]
  case class Element[A](value: A) extends Expression[A]

  object Expression:
    def l[A](elements: Expression[A]*): Expression[A] =
      ListOf(elements.toList)
    def e[A](element: A): Expression[A] =
      Element[A](element)

  type Packet = Expression[Int]
  case class PacketPair(left: Packet, right: Packet)


  @tailrec
  def parsePacket(input: List[String], position: Int, currentLists: List[ListOf[Int]], partialNumber: List[String]): Packet =
    if position == input.length then
      currentLists.head
    else
      input(position) match {
        case "[" =>
          parsePacket(input, position + 1, ListOf(List.empty) +: currentLists, List())
        case "]" =>
          val readyNumber = partialNumber.mkString
          val topListOf = currentLists.head
          val updatedTopList = if partialNumber.length > 0 then
            ListOf(topListOf.elements :+ Element(readyNumber.toInt))
          else
            topListOf
          val updatedParentList = currentLists.tail.headOption.map(parentList =>
            ListOf(elements = parentList.elements :+ updatedTopList)
          ).getOrElse(updatedTopList)
          parsePacket(input, position + 1, updatedParentList +: currentLists.drop(2), List())
        case "," =>
          val readyNumber = partialNumber.mkString
          val topListOf = currentLists.head
          val updatedTopList = if partialNumber.length > 0 then
            ListOf(topListOf.elements :+ Element(readyNumber.toInt))
          else
            topListOf
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

