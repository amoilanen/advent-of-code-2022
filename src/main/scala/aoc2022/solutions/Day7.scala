package aoc2022.solutions

import java.util.IllegalFormatException
import scala.collection.mutable

object Day7:

  sealed trait File(val name: String):
    val isDirectory: Boolean //TODO: How to get rid of this not very nice type class tag like method?
    lazy val computedSize: Int
    def getAllDirectories: List[File]

  /*
   * TODO: How might it be possible to easily build a tree without a var?
   * One approach is keeping more "folding state" in buildTree: mapping of file id to the actual file and children would then contain ids instead of actual files
   * in Directory
   */
  case class Directory(override val name: String, var children: List[File] = List.empty) extends File(name):
    override val isDirectory: Boolean = true
    lazy val computedSize: Int =
      children.map(_.computedSize).sum
    override def getAllDirectories: List[File] =
      this +: children.map(_.getAllDirectories).flatten

  case class RegularFile(override val name: String, size: Int) extends File(name):
    override val isDirectory: Boolean = false
    lazy val computedSize: Int =
      size
    override def getAllDirectories: List[File] =
      List.empty

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
    val directoryStack = List(Directory("/"))
    val commandsToInterpret = commands.drop(1) // drop "cd /
    val finalDirectoryStack = commandsToInterpret.foldLeft(directoryStack)((directoryStack, currentCommand) =>
      if (directoryStack.isEmpty)
        throw new IllegalStateException(s"Navigated away from the file system, directory stack cannot be empty, command $currentCommand")
      val currentDirectory = directoryStack.head
      currentCommand match {
        case CdUp =>
          directoryStack.drop(1)
        case CdToChildDirectory(directoryName) =>
          val childDirectory = currentDirectory.children.find(child =>
            child.name == directoryName && child.isDirectory
          )
          if (childDirectory.isEmpty)
            throw new IllegalStateException(s"Could not find directory name $childDirectory in directory '$currentDirectory', command '$currentCommand'")
          else
            childDirectory.get.asInstanceOf[Directory] +: directoryStack
        case Ls(foundChildren) =>
          currentDirectory.children = foundChildren
          directoryStack
      }
    )
    val rootDirectory = finalDirectoryStack.find(_.name == "/").get
    rootDirectory

  def getDirectoriesAndTheirSizesFrom(root: Directory): List[(String, Int)] =
    root.getAllDirectories.map(directory => (directory.name, directory.computedSize))

  def solutionPart1(parsed: List[Command]): Int =
    val root = buildTree(parsed)
    val directoriesWithSizes = getDirectoriesAndTheirSizesFrom(root)
    directoriesWithSizes.map(_._2).filter(_ <= 100000).sum

  val MinimalDiskSpaceNeededForUpdate = 30000000
  val TotalDiskSpace = 70000000

  def solutionPart2(parsed: List[Command]): Int =
    val root = buildTree(parsed)
    val rootSize = root.computedSize
    val remainingDiskSpace = TotalDiskSpace - rootSize
    val diskSpaceToFree = MinimalDiskSpaceNeededForUpdate - remainingDiskSpace
    if (diskSpaceToFree > 0) {
      val directoriesWithSizes = getDirectoriesAndTheirSizesFrom(root)
      directoriesWithSizes.map(_._2).filter(_ >= diskSpaceToFree).min
    } else
      -1

@main def day7Main: Unit =
  import Day7._
  import Day7Input._
  val parsed = parse(input)
  println(parsed)
  val tree = buildTree(parsed)
  println(tree)
  println("All directories")
  val sizesOfDirectories = getDirectoriesAndTheirSizesFrom(tree)
  println(sizesOfDirectories)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))