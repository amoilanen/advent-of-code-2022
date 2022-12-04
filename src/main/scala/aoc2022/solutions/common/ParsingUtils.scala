package aoc2022.solutions.common

object ParsingUtils:

  case class ParsingError(message: String, cause: Option[Throwable] = None)
    extends RuntimeException(message, cause.getOrElse(null))

  case class SplittingState[T](readyParts: Seq[Seq[T]] = Seq(), currentPart: Seq[T] = Seq())

  def splitBy[T](delimeter: T)(elements: Seq[T]): Seq[Seq[T]] =
    val finalSplittingState = elements.foldLeft(SplittingState[T]()) {
      case (SplittingState(readyParts, currentPart), element) =>
        if (element == delimeter)
          SplittingState(readyParts :+ currentPart, Seq())
        else
          SplittingState(readyParts, currentPart :+ element)
    }
    finalSplittingState.readyParts :+ finalSplittingState.currentPart
