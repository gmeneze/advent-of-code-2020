import InputReader._

object Day3 extends App {
  val ROW_SIZE = inputGrid.size
  val COL_SIZE = inputGrid.head.size
  type Grid = Vector[Vector[Char]]

  def isTree(c: Char): Boolean = c == '#'
  def isOpenSpace(c: Char): Boolean = c == '.'

  def solutionToFirstHalf(grid: Grid, right: Int, down: Int): Long = {
    var (curRow, curCol) = (0, 0)
    var treeCount = 0L

    grid.zipWithIndex.foreach { case (line, row) =>
      if (row == curRow) {
        val position = line(curCol % COL_SIZE)
        if (isTree(position)) treeCount += 1
        curRow += down
        curCol += right
      }
    }

    treeCount
  }

  def solutionToSecondHalf(grid: Grid): Long = {
    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map { case (right, down) =>
        solutionToFirstHalf(grid, right, down)
      }
      .reduce(_ * _)
  }

  lazy val inputGrid: Grid =
    readAllLines("day-3-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(_.toVector)
      .toVector

  println(solutionToFirstHalf(inputGrid, 3, 1))
  println(solutionToSecondHalf(inputGrid))
}
