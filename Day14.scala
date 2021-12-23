import InputReader._
import scala.util.matching.Regex

object Day14 extends App {
  case class Location(id: Long)
  case class Mask(value: List[Char])

  def clearBit(num: Long, bitId: Int): Long =
    num & ~(1L << bitId)

  def setBit(num: Long, bitId: Int): Long =
    num | (1L << bitId)

  def decodeValueV2(value: Long, mask: Mask): Vector[Long] = {
    def loop(mask: List[Char], index: Int, value: Long): Vector[Long] = {
      mask match {
        case Nil => Vector(value)
        case maskBit :: remainingBits =>
          maskBit match {
            case '0' => loop(remainingBits, index + 1, value)
            case '1' =>
              val updatedValue = setBit(value, index)
              loop(remainingBits, index + 1, updatedValue)
            case 'X' =>
              val updatedValueWith0 = clearBit(value, index)
              val updatedValueWith1 = setBit(value, index)

              val result0: Vector[Long] =
                loop(remainingBits, index + 1, updatedValueWith0)
              val result1: Vector[Long] =
                loop(remainingBits, index + 1, updatedValueWith1)
              result0 ++ result1
          }
      }
    }

    loop(mask.value.reverse, 0, value)
  }

  def decodeValue(value: Long, mask: Mask): Long = {
    var currValue = value

    mask.value.reverse.zipWithIndex.foreach { case (maskBit, bitId) =>
      maskBit match {
        case 'X' => // move on
        case '0' =>
          currValue = clearBit(currValue, bitId)
        case '1' =>
          currValue = clearBit(currValue, bitId)
          currValue = setBit(currValue, bitId)
      }
    }

    currValue
  }

  def solutionToFirstHalf(
      locationToValueList: List[(Location, (Long, Mask))]
  ) = {
    val locationToActualValue: Map[Location, Long] =
      locationToValueList.toMap.map { case (location, (value, mask)) =>
        location -> decodeValue(value, mask)
      }

    locationToActualValue.values.sum
  }

  def solutionToSecondHalf(
      locationToValueList: List[(Location, (Long, Mask))]
  ) = {
    val actualLocationsToValue: Map[Location, Long] = {
      val actualLocationToValueList: List[(Location, Long)] =
        locationToValueList.flatMap { case (location, (value, mask)) =>
          val actualLocationsToValue: Vector[(Location, Long)] =
            decodeValueV2(location.id, mask).map { actualLocationId =>
              Location(actualLocationId) -> value
            }
          actualLocationsToValue
        }
      actualLocationToValueList.toMap
    }

    actualLocationsToValue.values.sum
  }

  val locationToValueList: List[(Location, (Long, Mask))] = {
    val inputLines: List[String] =
      readAllLines("day-14-input.txt")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .toList

    val maskRegex: Regex = """mask = (\S+)""".r
    val locationAndValueRegex: Regex = """mem\[(\d+)\] = (\d+)""".r

    var currMask: Mask = null
    var locationToValueList: List[(Location, (Long, Mask))] = List.empty
    inputLines.foreach { str =>
      str match {
        case maskRegex(mask) =>
          currMask = Mask(mask.toList)
        case locationAndValueRegex(location, value) =>
          locationToValueList = locationToValueList.appended(
            Location(location.toLong) -> (value.toLong, currMask)
          )
        case _ => throw new IllegalArgumentException(s"input $str is invalid")
      }
    }

    locationToValueList
  }

  println(solutionToFirstHalf(locationToValueList))
  println(solutionToSecondHalf(locationToValueList))
}
