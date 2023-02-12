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

  case class ValveAchievedPressureAndOpenValves(pressure: Int, openValves: Set[ValveId])

  def findGreatestPressure(startValveId: ValveId, valves: Seq[Valve], totalMinutes: Int): Int =
    // Partially computed max possible pressures and the sets of open valves for each valve
    val state: MutableMap[ValveId, MutableMap[Int, ValveAchievedPressureAndOpenValves]] = MutableMap.empty

    valves.foreach(valve =>
      state(valve.id) = MutableMap.from(Map.empty)
    )
    state(startValveId) = MutableMap.from(Map(totalMinutes -> ValveAchievedPressureAndOpenValves(0, Set.empty)))

    def improveNextValveAchievedPressures(updatedRemainingMinutes: Int, updatedNextValveEstimate: ValveAchievedPressureAndOpenValves, nextValveIds: Seq[ValveId]): Unit =
      if updatedRemainingMinutes >= 0 then
        nextValveIds.foreach(nextValveId =>
          val nextValveState = state(nextValveId)
          nextValveState.get(updatedRemainingMinutes) match
            case Some(currentNextValveEstimate) =>
              if currentNextValveEstimate.pressure < updatedNextValveEstimate.pressure then
                nextValveState(updatedRemainingMinutes) = updatedNextValveEstimate
            case None =>
              nextValveState(updatedRemainingMinutes) = updatedNextValveEstimate
        )

    (1 to totalMinutes).foreach(_ =>
      valves.foreach(valve =>
        val valveState = state(valve.id)
        valveState.foreach({ case (remainingMinutes, ValveAchievedPressureAndOpenValves(pressure, openValves)) =>
          // First compute the improved estimates for the case when the current valve is open (if it is not already open)
          if !openValves.contains(valve.id) && valve.rate > 0 then
            val pressureIncreasedIfTurnedOn = (remainingMinutes - 1) * valve.rate // the pressure the current open valve will release until the moves end
            improveNextValveAchievedPressures(
              updatedRemainingMinutes = remainingMinutes - 2, // 1 move to open the current valve + 1 move to move to another valve
              updatedNextValveEstimate = ValveAchievedPressureAndOpenValves(
                pressure = pressure + pressureIncreasedIfTurnedOn,
                openValves = openValves + valve.id
              ),
              nextValveIds = valve.connections
            )
          // Compute the improved estimates for the case when the current valve is not open
          improveNextValveAchievedPressures(
            updatedRemainingMinutes = remainingMinutes - 1, // 1 move to move to another valve
            updatedNextValveEstimate = ValveAchievedPressureAndOpenValves(
              pressure,
              openValves
            ),
            nextValveIds = valve.connections
          )
        })
      )
    )

    val achievedMaxPressures = state.values.flatMap(_.filter({ case (moveNumber, _) =>
      moveNumber == 0
    })).map({ case (_, valveEstimate) =>
      //println(valveEstimate.pressure + ": " + valveEstimate.openValves)
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