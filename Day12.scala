import InputReader._

import scala.util.Try

object Day12 extends App {
  sealed trait Action
  case class MoveNorth(value: Int) extends Action
  case class MoveSouth(value: Int) extends Action
  case class MoveEast(value: Int) extends Action
  case class MoveWest(value: Int) extends Action
  case class MoveLeft(value: Int) extends Action
  case class MoveRight(value: Int) extends Action
  case class MoveFront(value: Int) extends Action

  object Action {
    def fromString(str: String): Try[Action] = Try {
      val pattern = """([NSEWLRF])(\d+)""".r

      str match {
        case pattern(dir, value) =>
          dir match {
            case "N" => MoveNorth(value.toInt)
            case "S" => MoveSouth(value.toInt)
            case "E" => MoveEast(value.toInt)
            case "W" => MoveWest(value.toInt)
            case "L" => MoveLeft(value.toInt)
            case "R" => MoveRight(value.toInt)
            case "F" => MoveFront(value.toInt)
          }
        case _ =>
          throw new IllegalArgumentException(
            "$str cannot be decoded into Action"
          )
      }
    }
  }

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  case class Position(x: Int, y: Int)

  case class Ship(pos: Position)

  case class WayPoint(relPos: Position) {
    def moveClockwise(degrees: Int): WayPoint = {
      val Position(x, y) = relPos
      degrees match {
        case 0 =>
          WayPoint(Position(x, y))
        case 90 =>
          WayPoint(Position(y, -x))
        case 180 =>
          WayPoint(Position(-x, -y))
        case 270 =>
          WayPoint(Position(-y, x))
        case 360 =>
          WayPoint(Position(x, y))
      }
    }

    def moveCounterClockwise(degrees: Int): WayPoint = {
      val Position(x, y) = relPos
      degrees match {
        case 0 =>
          WayPoint(Position(x, y))
        case 90 =>
          WayPoint(Position(-y, x))
        case 180 =>
          WayPoint(Position(-x, -y))
        case 270 =>
          WayPoint(Position(y, -x))
        case 360 =>
          WayPoint(Position(x, y))
      }
    }

    def isNorth: Boolean = relPos.y > 0
    def isSouth: Boolean = relPos.y < 0
    def isEast: Boolean = relPos.x > 0
    def isWest: Boolean = relPos.x < 0
  }

  case class Result(ship: Ship, wayPoint: WayPoint)

  def processAction(action: Action, ship: Ship, wayPoint: WayPoint): Result = {
    val Position(x, y) = wayPoint.relPos
    action match {
      case MoveNorth(value) =>
        Result(ship, WayPoint(wayPoint.relPos.copy(y = y + value)))
      case MoveSouth(value) =>
        Result(ship, WayPoint(wayPoint.relPos.copy(y = y - value)))
      case MoveEast(value) =>
        Result(ship, WayPoint(wayPoint.relPos.copy(x = x + value)))
      case MoveWest(value) =>
        Result(ship, WayPoint(wayPoint.relPos.copy(x = x - value)))
      case MoveLeft(value) => Result(ship, wayPoint.moveCounterClockwise(value))
      case MoveRight(value) => Result(ship, wayPoint.moveClockwise(value))
      case MoveFront(value) =>
        val xDelta = wayPoint.relPos.x * value
        val yDelta = wayPoint.relPos.y * value
        val Ship(Position(x, y)) = ship
        Result(Ship(Position(x + xDelta, y + yDelta)), wayPoint)
    }
  }

  def solutionToSecondHalf(actions: List[Action]): Long = {
    var ship = Ship(pos = Position(0, 0))
    var wayPoint = WayPoint(relPos = Position(10, 1))

    actions.foreach { action =>
      val result = processAction(action, ship, wayPoint)
      ship = result.ship
      wayPoint = result.wayPoint
    }

    val Ship(Position(x, y)) = ship
    math.abs(x) + math.abs(y)
  }

  val inputActions: List[Action] =
    readAllLines("day-12-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(s => Action.fromString(s).get)

  println(solutionToSecondHalf(inputActions))
}
