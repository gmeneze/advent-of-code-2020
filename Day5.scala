import InputReader._

import scala.util.Try

object Day5 extends App {
  case class Row(start: Int, end: Int)
  case class Col(start: Int, end: Int)

  sealed trait Instruction
  sealed trait RowInstruction extends Instruction
  case object Front extends RowInstruction
  case object Back extends RowInstruction
  sealed trait ColInstruction extends Instruction
  case object Left extends ColInstruction
  case object Right extends ColInstruction

  type BoardingPass = List[Instruction]

  case class SeatId(value: Long)

  object Instruction {
    def fromChar(char: Char): Try[Instruction] = Try {
      char match {
        case 'F' => Front
        case 'B' => Back
        case 'L' => Left
        case 'R' => Right
        case _ =>
          throw new IllegalArgumentException(
            s"char: $char is not an instruction"
          )
      }
    }
  }

  def processRowInstruction(row: Row, instruction: RowInstruction): Row = {
    val mid = (row.start + row.end) / 2

    instruction match {
      case Front => Row(row.start, mid)
      case Back  => Row(mid + 1, row.end)
    }
  }

  def processColInstruction(col: Col, instruction: ColInstruction): Col = {
    val mid = (col.start + col.end) / 2

    instruction match {
      case Left  => Col(col.start, mid)
      case Right => Col(mid + 1, col.end)
    }
  }

  def processBoardingPass(pass: BoardingPass): SeatId = {
    var row = Row(0, 127)
    var col = Col(0, 7)

    val (
      rowInstructions: List[RowInstruction],
      colInstructions: List[ColInstruction]
    ) =
      pass.partition(_.isInstanceOf[RowInstruction])

    rowInstructions.foreach { instruction =>
      row = processRowInstruction(row, instruction)
    }

    colInstructions.foreach { instruction =>
      col = processColInstruction(col, instruction)
    }

    require(row.start == row.end)
    require(col.start == col.end)

    SeatId(row.start * 8L + col.start)
  }

  def solutionToFirstHalf(boardingPasses: List[BoardingPass]): SeatId = {
    boardingPasses.map(processBoardingPass(_)).maxBy(_.value)
  }

  def solutionToSecondHalf(
      boardingPasses: List[BoardingPass]
  ): Option[SeatId] = {
    val sortedSeats: Vector[SeatId] =
      boardingPasses.map(processBoardingPass(_)).sortBy(_.value).toVector

    def gapFound(prev: Long, curr: Long): Boolean = curr != (prev + 1)

    sortedSeats.sliding(2, 1).collectFirst {
      case Vector(SeatId(prev), SeatId(curr)) if gapFound(prev, curr) =>
        SeatId(prev + 1)
    }
  }

  val boardingPasses: List[BoardingPass] =
    readAllLines("day-5-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toList.map(c => Instruction.fromChar(c).get))

  println(solutionToFirstHalf(boardingPasses))
  println(solutionToSecondHalf(boardingPasses).get)
}
