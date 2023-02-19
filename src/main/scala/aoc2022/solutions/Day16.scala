package aoc2022.solutions

import aoc2022.solutions.Day12.{Vertex, solutionPart2}
import aoc2022.solutions.common.ParsingUtils.ParsingError

import scala.collection.mutable.Map as MutableMap

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

  case class ValveAchievedPressureAndOpenValves(pressure: Int, openValves: Set[ValveId], openMinutes: Map[ValveId, Int])

  def findGreatestPressure(startValveId: ValveId, valves: Seq[Valve], totalMinutes: Int): Int =
    // Partially computed max possible pressures and the sets of open valves for each valve
    val state: MutableMap[ValveId, MutableMap[Int, ValveAchievedPressureAndOpenValves]] = MutableMap.empty
    val valveIdsToValve: Map[ValveId, Valve] = valves.map(valve => valve.id -> valve).toMap
    valves.foreach(valve =>
      state(valve.id) = MutableMap.from(Map.empty)
    )

    def updateAchievedValvePressures(valve: Valve, remainingMinutes: Int, achievedPressure: ValveAchievedPressureAndOpenValves): Unit =
      val ValveAchievedPressureAndOpenValves(pressure, openValves, openMinutes) = achievedPressure
      val valveState = state(valve.id)

      // Current valve is opened
      if !openValves.contains(valve.id) && valve.rate > 0 then
        val updatedRemainingMinutes = remainingMinutes - 1
        val pressureIncreasedIfTurnedOn = updatedRemainingMinutes * valve.rate
        val updatedAchievedPressure = achievedPressure.copy(
          pressure = pressure + pressureIncreasedIfTurnedOn,
          openValves = openValves + valve.id,
          openMinutes = openMinutes.updated(valve.id, updatedRemainingMinutes)
        )
        valveState.get(remainingMinutes - 1) match
          case Some(currentEstimate) =>
            if currentEstimate.pressure < updatedAchievedPressure.pressure then
              valveState(updatedRemainingMinutes) = updatedAchievedPressure
          case None =>
            valveState(updatedRemainingMinutes) = updatedAchievedPressure

      // Current valve is not opened
      valveState.get(remainingMinutes) match
        case Some(currentEstimate) =>
          if currentEstimate.pressure < achievedPressure.pressure then
            valveState(remainingMinutes) = achievedPressure
        case None =>
          valveState(remainingMinutes) = achievedPressure

    val startValve = valveIdsToValve(startValveId)
    updateAchievedValvePressures(startValve, totalMinutes, ValveAchievedPressureAndOpenValves(0, Set.empty, Map.empty))

    (1 to totalMinutes).foreach(_ =>
      valves.foreach(valve =>
        val valveState = state(valve.id)
        valveState.foreach({ case (remainingMinutes, achievedPressureEstimate) =>
          valve.connections.foreach(nextValveId =>
            val nextValve = valveIdsToValve(nextValveId)
            updateAchievedValvePressures(nextValve, remainingMinutes - 1, achievedPressureEstimate)
          )
        })
      )
    )

    val achievedMaxPressures = state.values.flatMap(_.filter({ case (moveNumber, _) =>
      moveNumber == 0
    })).map({ case (_, valveEstimate) =>
      println(valveEstimate.pressure + ": " + valveEstimate.openMinutes)
      valveEstimate.pressure
    })
    achievedMaxPressures.max

  val StartValveId = ValveId("AA")
  val TotalMoveNumber = 30

  def solutionPart1(valves: Seq[Valve]): Int =
    findGreatestPressure(StartValveId, valves, TotalMoveNumber)

@main
def day16Main: Unit =
  import Day16._
  import Day16Input._
  println("Day16")
  val parsed = parse(input)
  println(parsed)
  println(solutionPart1(parsed))

  val expectedAnswer = 20 * 28 + 13 * 25 + 21 * 21 + 22 * 13 + 3 * 9 + 2 * 6
  println(expectedAnswer)