package aoc2022.solutions

import java.util.IllegalFormatException

object Day7:

  sealed trait File(val name: String):
    val isDirectory: Boolean

  case class Directory(override val name: String, var children: List[File] = List.empty, var parent: Option[Directory] = None) extends File(name):
    override val isDirectory: Boolean = true
    def getRoot: Directory =
      parent.fold(this)(_.getRoot)

  case class RegularFile(override val name: String, size: Int) extends File(name):
    override val isDirectory: Boolean = false

  sealed trait Command
  case class Ls(output: List[File]) extends Command

  sealed trait Cd extends Command
  case class CdToChildDirectory(dirName: String) extends Cd
  object CdUp extends Cd:
    override def toString: String = "CdUp"

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

  def parse(input: String): List[Command] =
    val commands = input.split("\\$").map(_.trim).filter(_.nonEmpty).toList
    commands.map(parseCommand)

  def buildTree(commands: List[Command]): Directory =
    val rootDirectory = Directory("/")
    val commandsToInterpret = commands.drop(1) // drop "cd /
    val finalCurrentDirectory = commandsToInterpret.foldLeft(rootDirectory)((currentDirectory, currentCommand) =>
      println(s"Current directory: $currentDirectory")
      println(s"Current command: $currentCommand")
      currentCommand match {
        case CdUp =>
          if (currentDirectory.parent.isEmpty)
            throw new IllegalStateException(s"Tried to navigate up from a directory not having parent directory directory '$currentDirectory', command '$currentCommand'")
          else
            currentDirectory.parent.get
        case CdToChildDirectory(directoryName) =>
          val childDirectory = currentDirectory.children.find(child =>
            child.name == directoryName && child.isDirectory
          )
          if (childDirectory.isEmpty)
            throw new IllegalStateException(s"Could not find directory name $childDirectory in directory '$currentDirectory', command '$currentCommand'")
          else
            childDirectory.asInstanceOf[Option[Directory]].get
        case Ls(foundChildren) =>
          foundChildren.foreach({
            case child: Directory =>
              child.parent = Some(currentDirectory)
            case _: File =>
          })
          currentDirectory.children = foundChildren
          currentDirectory
      }
    )
    finalCurrentDirectory.getRoot

@main def day7Main: Unit =
  import Day7._
  import Day7Input._
  val parsed = parse(input)
  println(parsed)
  val tree = buildTree(parsed)
  println(tree)
