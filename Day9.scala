import InputReader._

import scala.util.Try

object Day9 extends App {
  def solutionToFirstHalf(
      numbers: List[Long],
      preambleSize: Int
  ): Option[Long] = {
    def checkIfPreambleContainsNos(
        target: Long,
        preambleNos: Set[Long]
    ): Boolean = {
      preambleNos.foreach { firstNum =>
        val secondNum = target - firstNum
        if ((firstNum != secondNum) && preambleNos.contains(secondNum))
          return true
      }
      return false
    }

    var preambleNos = Set.empty[Long]
    for ((num, index) <- numbers.zipWithIndex) {
      (preambleNos.size == preambleSize) match {
        case true =>
          if (!checkIfPreambleContainsNos(num, preambleNos)) return Some(num)
          val oldNum = numbers(index - preambleSize)
          preambleNos -= oldNum
          preambleNos += num
        case false =>
          preambleNos += num
      }
    }
    None
  }

  def solutionToSecondHalf(numbers: List[Long], preambleSize: Int): Long = {
    val invalidNum = solutionToFirstHalf(numbers, preambleSize).get
    println(s"invalid num is: $invalidNum")

    def findContinguousNos(numbers: List[Long], target: Long): Option[Long] = {
      var sum = 0L
      var contiguousList: List[Long] = List.empty[Long]

      for (num <- numbers) {
        contiguousList = contiguousList.appended(num)
        sum += num
        (sum == target) match {
          case true =>
            val minNum = contiguousList.min
            val maxNum = contiguousList.max
            return Some(minNum + maxNum)
          case false =>
            if (sum > target) return None
        }
      }

      return None
    }

    def loop(numbers: List[Long], target: Long): Try[Long] = Try {
      numbers match {
        case num :: nums =>
          val result: Option[Long] = findContinguousNos(numbers, target)
          if (result.isDefined) result.get
          else loop(nums, target).get
        case _ =>
          throw new IllegalArgumentException(
            s"no contiguous nos. sum to $target"
          )
      }
    }

    loop(numbers, invalidNum).get
  }

  val preambleSize = 25
  val numbers: List[Long] =
    readAllLines("day-9-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toLong)

  println(solutionToFirstHalf(numbers, preambleSize))
  println(solutionToSecondHalf(numbers, preambleSize))
}
