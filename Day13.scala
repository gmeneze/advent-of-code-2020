import InputReader._

object Day13 extends App {
  sealed trait Bus
  case class RegularBus(id: Int) extends Bus
  case object SpecialBus extends Bus

  object Bus {
    def fromString(str: String): Bus =
      str match {
        case id if id.forall(_.isDigit) => RegularBus(id.toInt)
        case "x"                        => SpecialBus
        case _ =>
          throw new IllegalArgumentException(
            s"invalid input: $str"
          ) // these are invalid
      }
  }

  def solutionToFirstHalf(departMinutes: Int, buses: Vector[Bus]): Long = {
    val regularBuses: Vector[RegularBus] = buses.collect {
      case regular: RegularBus => regular
    }

    val minutesToBus: Vector[(Int, RegularBus)] =
      regularBuses
        .map { bus =>
          val minutesToBus = {
            val modulus: Int = departMinutes % bus.id
            bus.id - modulus
          }
          (minutesToBus, bus)
        }

    val (waitMinutes: Int, bus: RegularBus) = minutesToBus.sortBy(_._1).head
    waitMinutes.toLong * bus.id
  }

  def solutionToSecondHalf(buses: Vector[Bus]): Long = {
    def doesBusDepart(time: Long, offset: Int, bus: RegularBus): Boolean =
      (time + offset) % bus.id == 0L

    // very creative solution: https://www.youtube.com/watch?v=4_5mluiXF5I
    var time = 0L
    var stepSize = 1L
    for {
      (bus, offset) <- buses.zipWithIndex
    } {
      bus match {
        case SpecialBus => // move on to next bus
        case b @ RegularBus(busId) =>
          while (!doesBusDepart(time, offset, b)) {
            time += stepSize
          }

          stepSize *= busId
      }
    }

    time
  }

  val (departMinutes: Int, buses: Vector[Bus]) = {
    val inputLines: List[String] =
      readAllLines("day-13-input.txt")
        .map(_.trim)
        .filterNot(_.isEmpty)

    inputLines match {
      case List(departMinStr, busesStr) =>
        val departMinutes: Int = departMinStr.toInt
        val buses: Vector[Bus] =
          busesStr
            .split(",")
            .map(_.trim)
            .filterNot(_.isEmpty)
            .map(Bus.fromString)
            .toVector
        (departMinutes, buses)
      case _ => throw new IllegalArgumentException(s"invalid input $inputLines")
    }
  }

  println(solutionToFirstHalf(departMinutes, buses))
  println(solutionToSecondHalf(buses))
}
