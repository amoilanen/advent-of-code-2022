package aoc2022.solutions

import aoc2022.solutions.Day12.{Vertex, solutionPart2}
import aoc2022.solutions.common.ParsingUtils.ParsingError

import scala.collection.mutable.Map as MutableMap
import scala.collection.mutable.Set as MutableSet
import scala.collection.mutable.Queue as MutableQueue

object Day16:

  case class ValveId(value: String) extends AnyVal
  case class Valve(id: ValveId, rate: Int, connections: Seq[ValveId])

  def parseValve(valveInput: String): Valve =
    def asValve(name: String, rate: String, toValves: String): Valve =
      val connections = toValves.split(", ").map(ValveId(_))
      Valve(ValveId(name), rate.toInt, connections)
    valveInput match
      case s"Valve $name has flow rate=$rate; tunnels lead to valves $toValves" =>
        asValve(name, rate, toValves)
      case s"Valve $name has flow rate=$rate; tunnel leads to valve $toValves" =>
        asValve(name, rate, toValves)
      case _ =>
        throw ParsingError(s"Could not parse valve input '$valveInput'")

  def parse(input: String): Seq[Valve] =
    input.split("\n").map(_.trim).filter(_.nonEmpty).map(parseValve).toSeq

  def findShortestPaths(valves: Seq[Valve]): Map[(ValveId, ValveId), Int] =
    val distances = MutableMap.from(Map.empty[(ValveId, ValveId), Int])
    val valveIds = valves.map(_.id)
    for
      from <- valveIds
      to <- valveIds
    yield
      distances.put((from, to), Int.MaxValue / 3)
    for
      from <- valveIds
    yield
      distances.put((from, from), 0)
    for
      from <- valves
      to <- from.connections
    yield
      distances.put((from.id, to), 1)
    for
      through <- valveIds
      from <- valveIds
      to <- valveIds
    yield
      if distances((from, to)) > distances((from, through)) + distances((through, to)) then
        distances((from, to)) = distances((from, through)) + distances((through, to))
    distances.toMap

  def findGreatestPressure(startValve: Valve, valves: Seq[Valve], totalMinutes: Int): Int =
    val nonZeroRateValves: Set[Valve] = valves.filter(valve =>
      valve.rate > 0 && valve != startValve
    ).toSet
    val shortestPaths: Map[(ValveId, ValveId), Int] = findShortestPaths(valves)

    //TODO: The rate of nextValve is though always > 0?
    def maxReleasedPressure(currentValve: Valve, remainingValvesToOpen: Set[Valve], pressureReleasedSoFar: Int, currentMinute: Int): Int =
      if currentMinute < totalMinutes && remainingValvesToOpen.nonEmpty then
        val possibleReleasedPressures = remainingValvesToOpen.map(nextValve =>
          val minutesToMove = shortestPaths((currentValve.id, nextValve.id))
          val updatedPressureReleasedSoFar = if nextValve.rate > 0 then
            pressureReleasedSoFar + nextValve.rate * (totalMinutes - currentMinute - minutesToMove)
          else
            pressureReleasedSoFar
          val updatedCurrentMinute =
            if nextValve.rate > 0 then
              currentMinute + 1 + minutesToMove
            else
              currentMinute + minutesToMove
          val updatedRemainingValves = remainingValvesToOpen - nextValve
          maxReleasedPressure(nextValve, updatedRemainingValves, updatedPressureReleasedSoFar, updatedCurrentMinute)
        )
        possibleReleasedPressures.max
      else
        pressureReleasedSoFar
    // No need to open start valve: it always has rate 0
    maxReleasedPressure(startValve, nonZeroRateValves, 0, 1)

  val StartValveId = ValveId("AA")
  val TotalMoveNumber = 30

  def solutionPart1(valves: Seq[Valve]): Int =
    valves.find(_.id == StartValveId) match
      case None => throw new IllegalStateException(s"Could not find start valve $StartValveId")
      case Some(startValve) =>
        findGreatestPressure(startValve, valves, TotalMoveNumber)

@main
def day16Main: Unit =
  import Day16._
  import Day16Input._
  println("Day16")
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))