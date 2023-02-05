package aoc2022.solutions

import aoc2022.solutions.Day12.Vertex
import aoc2022.solutions.common.ParsingUtils.ParsingError
import scala.collection.mutable.{ Map => MutableMap }

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

  case class ValveMaxPressureAndOpenValves(pressure: Int, openValves: Set[ValveId])

  def findGreatestPressure(valves: Seq[Valve], totalMoveNumber: Int): Int =
    // Partially computed max possible pressures and the sets of open valves for each valve
    val state: MutableMap[ValveId, MutableMap[Int, ValveMaxPressureAndOpenValves]] = MutableMap.empty
    valves.foreach(valve =>
      state(valve.id) = MutableMap.from(Map(totalMoveNumber -> ValveMaxPressureAndOpenValves(0, Set.empty)))
    )

    def improveNextValveAchievedPressures(updatedMoveNumber: Int, updatedPressure: Int, updatedOpenValves: Set[ValveId], nextValveIds: Seq[ValveId]): Unit =
      nextValveIds.foreach(nextValveId =>
        val nextValveState = state(nextValveId)
        if nextValveState.contains(updatedMoveNumber) then
          val currentNextValveBestPressure = nextValveState(updatedMoveNumber)
          if currentNextValveBestPressure.pressure < updatedPressure then
            nextValveState(updatedMoveNumber) = ValveMaxPressureAndOpenValves(updatedPressure, updatedOpenValves)
        else
          nextValveState(updatedMoveNumber) = ValveMaxPressureAndOpenValves(updatedPressure, updatedOpenValves)
      )

    (1 to totalMoveNumber).foreach(_ =>
      valves.foreach(valve =>
        val valveState = state(valve.id)
        valveState.foreach({ case (moveNumber, ValveMaxPressureAndOpenValves(pressure, openValves)) =>
          val nextValveIds = valve.connections
          // First compute the improved estimates for the case when the current valve is open (if it is not already open)
          if !openValves.contains(valve.id) then
            val pressureIncreasedIfTurnedOn = (moveNumber - 1) * valve.rate // the pressure the current open valve will release until the moves end
            improveNextValveAchievedPressures(
              updatedMoveNumber = moveNumber - 2, // 1 move to open the current valve + 1 move to move to another valve
              updatedPressure = pressure + pressureIncreasedIfTurnedOn,
              updatedOpenValves = openValves + valve.id,
              nextValveIds = nextValveIds
            )
          // Compute the improved estimates for the case when the current valve is not open
          improveNextValveAchievedPressures(
            updatedMoveNumber = moveNumber - 1, // 1 move to move to another valve
            updatedPressure = pressure,
            updatedOpenValves = openValves,
            nextValveIds = nextValveIds
          )
        })
      )
    )

    //TODO: Iterate through the valve states and see whether some valves contain move number 0 and what would be the best possible estimate
    //among all those achieved 0s
    ???

@main
def day16Main: Unit =
  import Day16._
  import Day16Input._
  println("Day16")
  val parsed = parse(input)
  println(parsed)
