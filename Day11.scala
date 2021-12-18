import InputReader._

object Day11 extends App {
  type Grid = Vector[Vector[Char]]

  def isEmptySeat(char: Char): Boolean = char == 'L'
  def isOccupiedSeat(char: Char): Boolean = char == '#'
  def isFloor(char: Char): Boolean = char == '.'

  case class Result(newGrid: Grid, gridUpdated: Boolean)

  def processRound(
      grid: Grid,
      getAdjacentOccupiedSeatCount: (Grid, Int, Int) => Int,
      maxOccupiedSeats: Int
  ): Result = {
    var newGrid = grid
    val MAX_ROW = grid.size - 1
    val MAX_COL = grid(0).size - 1

    var updated = false

    for {
      row <- 0 to MAX_ROW
      col <- 0 to MAX_COL
    } {
      val currentSeat = grid(row)(col)
      if (!isFloor(currentSeat)) {
        val adjacentOccupiedSeatCount: Int =
          getAdjacentOccupiedSeatCount(grid, row, col)

        if (isEmptySeat(currentSeat) && adjacentOccupiedSeatCount == 0) {
          updated = true
          val newRow = newGrid(row).updated(col, '#')
          newGrid = newGrid.updated(row, newRow)
        } else if (
          isOccupiedSeat(
            currentSeat
          ) && adjacentOccupiedSeatCount >= maxOccupiedSeats
        ) {
          updated = true
          val newRow = newGrid(row).updated(col, 'L')
          newGrid = newGrid.updated(row, newRow)
        }
      }
    }

    Result(newGrid, updated)
  }

  def solutionToFirstHalf(grid: Grid): Int = {
    def getAdjacentOccupiedSeatCount(grid: Grid, row: Int, col: Int): Int = {
      val MAX_ROW = grid.size - 1
      val MAX_COL = grid(0).size - 1

      var count = 0
      for {
        i <- -1 to 1
        j <- -1 to 1
        newRow = row + i
        newCol = col + j
        if !(newRow == row && newCol == col)
        if (newRow >= 0 && newRow <= MAX_ROW)
        if (newCol >= 0 && newCol <= MAX_COL)
      } {
        val seat = grid(newRow)(newCol)
        if (isOccupiedSeat(seat)) count += 1
      }

      count
    }

    var gridUpdated = true
    var currGrid = grid

    while (gridUpdated) {
      val result = processRound(currGrid, getAdjacentOccupiedSeatCount, 4)
      currGrid = result.newGrid
      gridUpdated = result.gridUpdated
    }

    currGrid.map { row =>
      row.count(seat => isOccupiedSeat(seat))
    }.sum
  }

  def solutionToSecondHalf(grid: Grid): Int = {
    def getAdjacentOccupiedSeatCount(grid: Grid, row: Int, col: Int): Int = {
      val MAX_ROW = grid.size - 1
      val MAX_COL = grid(0).size - 1

      var count = 0

      def isFirstSeatInDirectionOccupied(
          row: Int,
          col: Int,
          rowChange: Int,
          colChange: Int
      ): Int = {
        val newRow = row + rowChange
        val newCol = col + colChange

        if (newRow < 0 || newRow > MAX_ROW) return 0
        if (newCol < 0 || newCol > MAX_COL) return 0

        val seat = grid(newRow)(newCol)
        isFloor(seat) match {
          case true =>
            isFirstSeatInDirectionOccupied(newRow, newCol, rowChange, colChange)
          case false =>
            if (isOccupiedSeat(seat)) 1
            else 0
        }
      }

      for {
        i <- -1 to 1
        j <- -1 to 1
        if !(i == 0 && j == 0)
      } {
        count += isFirstSeatInDirectionOccupied(row, col, i, j)
      }

      count
    }

    var gridUpdated = true
    var currGrid = grid

    while (gridUpdated) {
      val result = processRound(currGrid, getAdjacentOccupiedSeatCount, 5)
      currGrid = result.newGrid
      gridUpdated = result.gridUpdated
    }

    currGrid.map { row =>
      row.count(seat => isOccupiedSeat(seat))
    }.sum
  }

  val inputGrid: Grid =
    readAllLines("day-11-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toVector)
      .toVector

  println(solutionToFirstHalf(inputGrid))
  println(solutionToSecondHalf(inputGrid))

}
