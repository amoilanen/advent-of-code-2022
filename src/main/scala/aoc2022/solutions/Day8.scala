package aoc2022.solutions

object Day8:

  case class Grid(values: Array[Array[Int]]):
    val columnCount = values.headOption.map(_.size).getOrElse(0)
    val rowCount = values.size
    override def toString: String =
      val valuesRepresentation = values.map(_.mkString(" ")).mkString("\n")
      s"""
        |Grid [
        |$valuesRepresentation
        |]
        |""".stripMargin
    def min(other: Grid): Grid =
      val minValues = (0 until rowCount).map(rowIndex =>
        (0 until columnCount).map(columnIndex =>
          Math.min(values(rowIndex)(columnIndex), other.values(rowIndex)(columnIndex))
        ).toArray
      ).toArray
      Grid(minValues)
    def count(predicate: Int => Boolean): Int =
      values.foldLeft(0)((count, column) =>
        count + column.filter(predicate).size
      )

    def scenicScore(rowIndex: Int, columnIndex: Int): Int =
      val value = values(rowIndex)(columnIndex)
      val lowerToBottom = (rowIndex + 1 to rowCount - 1).takeWhile(idx => values(idx)(columnIndex) < value).size
      val visibleToBottom = if (lowerToBottom < rowCount - rowIndex - 1) // (rowIndex + 1 to rowCount - 1).size
        lowerToBottom + 1
      else
        lowerToBottom
      val lowerToTop = (0 to rowIndex - 1).reverse.takeWhile(idx => values(idx)(columnIndex) < value).size
      val visibleToTop = if (lowerToTop < rowIndex) // (0 to rowIndex - 1).size
        lowerToTop + 1
      else
        lowerToTop
      val lowerToRight = (columnIndex + 1 to columnCount - 1).takeWhile(idx => values(rowIndex)(idx) < value).size
      val visibleToRight = if (lowerToRight < columnCount - columnIndex - 1) // (columnIndex + 1 to columnCount - 1).size
        lowerToRight + 1
      else
        lowerToRight
      val lowerToLeft = (0 to columnIndex - 1).reverse.takeWhile(idx => values(rowIndex)(idx) < value).size
      val visibleToLeft = if (lowerToLeft < columnIndex) // (0 to columnIndex - 1).size
        lowerToLeft + 1
      else
        lowerToLeft
      visibleToLeft * visibleToRight * visibleToTop * visibleToBottom

    def bestScenicScore: Int =
      val scenicScores =
        (0 until rowCount).flatMap(rowIndex =>
          (0 until columnCount).map(columnIndex =>
            scenicScore(rowIndex, columnIndex)
          )
        )
      scenicScores.max

  def minBoundary(boundaries: Grid*): Grid =
    boundaries.reduce((x, y) => x.min(y))

  def parse(input: String): Grid =
    val values = input.split("\n").map(_.trim).filter(_.nonEmpty).map(
      _.split("").map(_.toInt)
    )
    Grid(values)

  def leftBoundary(grid: Grid): Grid =
    val rowCount = grid.rowCount
    val columnCount = grid.columnCount
    val boundary = Array.ofDim[Int](rowCount, columnCount)
    (0 until rowCount).foreach(idx =>
      boundary(idx)(0) = 0
    )
    for
      rowIndex <- 0 until rowCount
      columnIndex <- 1 until columnCount
    yield
      val value = Math.max(grid.values(rowIndex)(columnIndex - 1), boundary(rowIndex)(columnIndex - 1))
      boundary(rowIndex)(columnIndex) = value
    Grid(boundary)

  def rightBoundary(grid: Grid): Grid =
    val rowCount = grid.rowCount
    val columnCount = grid.columnCount
    val boundary = Array.ofDim[Int](rowCount, columnCount)
    (0 until rowCount).foreach(idx =>
      boundary(idx)(columnCount - 1) = 0
    )
    for
      rowIndex <- 0 until rowCount
      columnIndex <- (0 to columnCount - 2).reverse
    yield
      val value = Math.max(grid.values(rowIndex)(columnIndex + 1), boundary(rowIndex)(columnIndex + 1))
      boundary(rowIndex)(columnIndex) = value
    Grid(boundary)

  def topBoundary(grid: Grid): Grid =
    val rowCount = grid.rowCount
    val columnCount = grid.columnCount
    val boundary = Array.ofDim[Int](rowCount, columnCount)
    (0 until columnCount).foreach(idx =>
      boundary(0)(idx) = 0
    )
    for
      columnIndex <- 0 until columnCount
      rowIndex <- 1 until rowCount
    yield
      val value = Math.max(grid.values(rowIndex - 1)(columnIndex), boundary(rowIndex - 1)(columnIndex))
      boundary(rowIndex)(columnIndex) = value
    Grid(boundary)

  def bottomBoundary(grid: Grid): Grid =
    val rowCount = grid.rowCount
    val columnCount = grid.columnCount
    val boundary = Array.ofDim[Int](rowCount, columnCount)
    (0 until columnCount).foreach(idx =>
      boundary(rowCount - 1)(idx) = 0
    )
    for
      columnIndex <- 0 until columnCount
      rowIndex <- (0 to rowCount - 2).reverse
    yield
      val value = Math.max(grid.values(rowIndex + 1)(columnIndex), boundary(rowIndex + 1)(columnIndex))
      boundary(rowIndex)(columnIndex) = value
    Grid(boundary)

  def computeVisibleBoundary(trees: Grid): Grid =
    val directionBoundaries = List(leftBoundary, rightBoundary, topBoundary, bottomBoundary).map(_(trees))
    minBoundary(directionBoundaries:_*)

  def visibleTrees(trees: Grid): Grid =
    val boundary = computeVisibleBoundary(trees)
    val visibility = (0 until trees.rowCount).map(rowIndex =>
      (0 until trees.columnCount).map(columnIndex =>
        if (rowIndex == 0 || rowIndex == trees.rowCount - 1 || columnIndex == 0 || columnIndex == trees.columnCount - 1)
          1
        else if (trees.values(rowIndex)(columnIndex) > boundary.values(rowIndex)(columnIndex))
          1
        else
          0
      ).toArray
    ).toArray
    Grid(visibility)

  // Algorithm complexity θ(n^2) where n is the side of the grid
  def solutionPart1(trees: Grid): Int =
    visibleTrees(trees).count(_ > 0)

  // Algorithm complexity θ(n^3) where n is the side of the grid
  def solutionPart2(trees: Grid): Int =
    trees.bestScenicScore

@main def day8Main: Unit =
  import Day8._
  import Day8Input._
  val parsed = parse(input)
  println(solutionPart1(parsed))
  println(solutionPart2(parsed))

