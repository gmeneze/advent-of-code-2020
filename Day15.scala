import InputReader._

object Day15 extends App {

  def solutionToFirstHalf(startingNumbers: Vector[Long], turns: Long): Long = {
    var lastNum: Long = 0

    var numToLastOccurence: Map[Long, Long] =
      startingNumbers.zipWithIndex.map { case (num, idx) =>
        lastNum = num
        val turnId = (idx + 1).toLong
        num -> turnId
      }.toMap

    val startTurn: Long = startingNumbers.size + 1

    for {
      turn <- startTurn to turns
    } {
      val lastNumTurn: Long = turn - 1
      val currNum = {
        if (!numToLastOccurence.contains(lastNum)) 0L
        else {
          val prevOccurence: Long = numToLastOccurence(lastNum)
          lastNumTurn - prevOccurence
        }
      }
      numToLastOccurence += (lastNum -> lastNumTurn)
      lastNum = currNum
    }

    lastNum
  }

  println(solutionToFirstHalf(Vector(0, 3, 6), 2020))
  println(solutionToFirstHalf(Vector(0, 8, 15, 2, 12, 1, 4), 30000000))
}
