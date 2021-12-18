import InputReader._
import scala.util.Try

object Day8 extends App {
  sealed trait Operation
  case class Nop(argument: Int) extends Operation
  case class Acc(argument: Int) extends Operation
  case class Jmp(argument: Int) extends Operation

  object Operation {
    def fromString(str: String): Try[Operation] = Try {
      val nopRegex = """nop ([+-])(\d+)""".r
      val accRegex = """acc ([+-])(\d+)""".r
      val jmpRegex = """jmp ([+-])(\d+)""".r

      def getArgument(sign: String, arg: String): Int = {
        val argument = arg.toInt
        if (sign == "-") -1 * argument
        else argument
      }

      str match {
        case nopRegex(sign, arg) => Nop(getArgument(sign, arg))
        case accRegex(sign, arg) => Acc(getArgument(sign, arg))
        case jmpRegex(sign, arg) => Jmp(getArgument(sign, arg))
        case _ =>
          throw new IllegalArgumentException(s"$str is invalid operation")
      }
    }
  }

  case class ValidateOperations(infiniteLoop: Boolean, total: Long)
  def validateOperations(operations: Vector[Operation]): ValidateOperations = {
    def loop(
        position: Int,
        total: Long,
        seenPositions: Set[Int]
    ): ValidateOperations = {
      if (seenPositions.contains(position))
        return ValidateOperations(true, total)
      if (position == operations.size) return ValidateOperations(false, total)

      val operation = operations(position)

      operation match {
        case Nop(_) => loop(position + 1, total, seenPositions + position)
        case Acc(arg) =>
          loop(position + 1, total + arg, seenPositions + position)
        case Jmp(arg) => loop(position + arg, total, seenPositions + position)
      }
    }

    loop(0, 0L, Set.empty[Int])
  }

  def solutionToFirstHalf(operations: Vector[Operation]): Long = {
    validateOperations(operations).total
  }

  def solutionToSecondHalf(operations: Vector[Operation]): Option[Long] = {
    val operationsWithIndex: Vector[(Operation, Int)] = operations.zipWithIndex

    for ((operation, index) <- operationsWithIndex) {
      operation match {
        case Nop(arg) =>
          // change Nop to Jmp
          val newOperations = operations.updated(index, Jmp(arg))
          val ValidateOperations(isInfiniteLoop, acc) = validateOperations(
            newOperations
          )
          if (!isInfiniteLoop) return Some(acc)
        case Jmp(arg) =>
          // change Jmp to Nop
          val newOperations = operations.updated(index, Nop(arg))
          val ValidateOperations(isInfiniteLoop, acc) = validateOperations(
            newOperations
          )
          if (!isInfiniteLoop) return Some(acc)
        case _ => // no action needed, move on to next operation
      }
    }

    return None

  }

  val operations: Vector[Operation] =
    readAllLines("day-8-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(str => Operation.fromString(str).get)
      .toVector

  println(solutionToFirstHalf(operations))
  println(solutionToSecondHalf(operations).get)
}
