package aoc2022.solutions

import java.util.IllegalFormatException

object Day7:

  sealed trait File
  case class Directory(name: String) extends File
  case class RegularFile(name: String, size: Int) extends File

  sealed trait Command
  case class Ls(output: List[File]) extends Command

  sealed trait Cd extends Command
  case class CdToChildDirectory(dirName: String) extends Cd
  object CdUp extends Cd:
    override def toString: String = "CdUp"

  case class Commands(commands: List[Command])

  def parseFile(input: String): File =
    val Array(left, right) = input.split(" ")
    val fileName = right
    if (left == "dir")
      Directory(fileName)
    else
      val fileSize = left.toInt
      RegularFile(fileName, fileSize)

  def parseCommand(input: String): Command =
    if (input == "cd ..")
      CdUp
    else if (input.startsWith("cd"))
      val Array(_, directoryName) = input.split(" ")
      CdToChildDirectory(directoryName)
    else if (input.startsWith("ls"))
      val files = input.split("\n").drop(1).map(parseFile).toList
      Ls(files)
    else
      throw RuntimeException(s"Could not parse command '$input'")

  def parse(input: String): Commands =
    val commands = input.split("\\$").map(_.trim).filter(_.nonEmpty).toList
    commands.foreach(command =>
      println(s"'$command'")
    )
    Commands(commands.map(parseCommand))

@main def day7Main: Unit =
  import Day7._
  import Day7Input._
  val parsed = parse(input)
  println(parsed)
