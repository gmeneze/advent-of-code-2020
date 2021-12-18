import InputReader._
import scala.collection.mutable
object Day10 extends App {
  type Joltage = Int
  def solutionToFirstHalf(joltages: List[Joltage]): Long = {
    val allJoltages: List[Joltage] = joltages.prepended(0)
    val sortedJoltages: List[Joltage] = allJoltages.sorted

    case class Result(diffOne: Long, diffThree: Long)
    val Result(oneJoltDiff, threeJoltDiff) =
      sortedJoltages.sliding(2, 1).foldLeft(Result(0L, 0L)) {
        case (acc, List(first, second)) =>
          val diff = second - first
          diff match {
            case 1 => Result(acc.diffOne + 1, acc.diffThree)
            case 3 => Result(acc.diffOne, acc.diffThree + 1)
            case _ => acc
          }
      }

    oneJoltDiff * (threeJoltDiff + 1)
  }

  // test input:
  // 1 -> 2
  // sorted joltages: 0 -> 1 -> 2 -> 5
  // cache(2) = 1
  // cache(1) = 1
  // cache(0) = 2

  def solutionToSecondHalf(joltages: List[Int]): Long = {
    val deviceJoltage: Joltage = joltages.max + 3
    val allJoltages: List[Joltage] =
      joltages.prepended(0).prepended(deviceJoltage)
    val sortedJoltages: List[Joltage] = allJoltages.sorted

    val numPathsToDeviceCache: mutable.Map[Joltage, Long] =
      mutable.Map.empty[Joltage, Long]

    def loop(remainingJoltages: List[Joltage]): Long = {
      if (remainingJoltages.head == deviceJoltage) return 1L

      val currentJoltage = remainingJoltages.head
      if (numPathsToDeviceCache.contains(currentJoltage))
        return numPathsToDeviceCache(currentJoltage)

      var restOfJoltages = remainingJoltages.tail
      var numPaths = 0L

      def shouldContinue(restOfJoltages: List[Joltage]) = {
        def areJoltagesRemaining(restOfJoltages: List[Joltage]): Boolean =
          !restOfJoltages.isEmpty
        def isHeadJoltageValid(restOfJoltages: List[Joltage]): Boolean =
          (restOfJoltages.head - currentJoltage <= 3)

        areJoltagesRemaining(restOfJoltages) && isHeadJoltageValid(
          restOfJoltages
        )
      }

      while (shouldContinue(restOfJoltages)) {
        numPaths += loop(restOfJoltages)
        restOfJoltages = restOfJoltages.tail
      }

      numPathsToDeviceCache(currentJoltage) = numPaths
      numPathsToDeviceCache(currentJoltage)
    }

    loop(sortedJoltages)
  }

  val joltages: List[Int] =
    readAllLines("day-10-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toInt)

  println(solutionToFirstHalf(joltages))
  println(solutionToSecondHalf(joltages))
}
