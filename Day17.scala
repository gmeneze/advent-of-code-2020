import InputReader._

object Day17 extends App {
  case class Cube(x: Int, y: Int, z: Int) {
    def toHyperCube(w: Int): HyperCube = HyperCube(x, y, z, w)
  }

  case class HyperCube(x: Int, y: Int, z: Int, w: Int)

  case class Range(
      xMin: Int,
      xMax: Int,
      yMin: Int,
      yMax: Int,
      zMin: Int,
      zMax: Int
  ) {
    def toHyperRange(wMin: Int, wMax: Int): HyperRange =
      HyperRange(xMin, xMax, yMin, yMax, zMin, zMax, wMin, wMax)
  }

  case class HyperRange(
      xMin: Int,
      xMax: Int,
      yMin: Int,
      yMax: Int,
      zMin: Int,
      zMax: Int,
      wMin: Int,
      wMax: Int
  )

  sealed trait State
  case object Active extends State
  case object InActive extends State

  def solutionToFirstHalf(range: Range, activeCubes: Set[Cube]): Long = {
    def processCycle(range: Range, activeCubes: Set[Cube]): Set[Cube] = {
      val Range(x1, x2, y1, y2, z1, z2) = range

      var activeCubesAfterCycle: Set[Cube] = Set.empty[Cube]

      def getNeighbors(cube: Cube): IndexedSeq[Cube] =
        for {
          i <- -1 to 1
          j <- -1 to 1
          k <- -1 to 1
          nCube = Cube(cube.x + i, cube.y + j, cube.z + k)
          if nCube != cube
        } yield nCube

      for {
        x <- x1 to x2
        y <- y1 to y2
        z <- z1 to z2
      } {
        val currCube = Cube(x, y, z)
        val currCubeState = {
          activeCubes.contains(currCube) match {
            case true  => Active
            case false => InActive
          }
        }

        val activeNeighborCount: Int =
          getNeighbors(currCube).count(cube => activeCubes.contains(cube))

        currCubeState match {
          case Active =>
            if (activeNeighborCount == 2 || activeNeighborCount == 3)
              activeCubesAfterCycle += currCube
          case InActive =>
            if (activeNeighborCount == 3)
              activeCubesAfterCycle += currCube
        }
      }

      activeCubesAfterCycle
    }

    var currActiveCubes = activeCubes
    var currRange = range

    for (_ <- 1 to 6) {
      currRange = Range(
        currRange.xMin - 1,
        currRange.xMax + 1,
        currRange.yMin - 1,
        currRange.yMax + 1,
        currRange.zMin - 1,
        currRange.zMax + 1
      )

      currActiveCubes = processCycle(currRange, currActiveCubes)
    }

    currActiveCubes.size
  }

  def solutionToSecondHalf(
      range: HyperRange,
      activeCubes: Set[HyperCube]
  ): Long = {
    def processCycle(
        range: HyperRange,
        activeCubes: Set[HyperCube]
    ): Set[HyperCube] = {
      val HyperRange(x1, x2, y1, y2, z1, z2, w1, w2) = range

      var activeCubesAfterCycle: Set[HyperCube] = Set.empty[HyperCube]

      def getNeighbors(cube: HyperCube): IndexedSeq[HyperCube] =
        for {
          i <- -1 to 1
          j <- -1 to 1
          k <- -1 to 1
          l <- -1 to 1
          nCube = HyperCube(cube.x + i, cube.y + j, cube.z + k, cube.w + l)
          if nCube != cube
        } yield nCube

      for {
        x <- x1 to x2
        y <- y1 to y2
        z <- z1 to z2
        w <- w1 to w2
      } {
        val currCube = HyperCube(x, y, z, w)
        val currCubeState = {
          activeCubes.contains(currCube) match {
            case true  => Active
            case false => InActive
          }
        }

        val activeNeighborCount: Int =
          getNeighbors(currCube).count(cube => activeCubes.contains(cube))

        currCubeState match {
          case Active =>
            if (activeNeighborCount == 2 || activeNeighborCount == 3)
              activeCubesAfterCycle += currCube
          case InActive =>
            if (activeNeighborCount == 3)
              activeCubesAfterCycle += currCube
        }
      }

      activeCubesAfterCycle
    }

    var currActiveCubes = activeCubes
    var currRange = range

    for (_ <- 1 to 6) {
      currRange = HyperRange(
        currRange.xMin - 1,
        currRange.xMax + 1,
        currRange.yMin - 1,
        currRange.yMax + 1,
        currRange.zMin - 1,
        currRange.zMax + 1,
        currRange.wMin - 1,
        currRange.wMax + 1
      )

      currActiveCubes = processCycle(currRange, currActiveCubes)
    }

    currActiveCubes.size
  }

  val (activeCubes: Set[Cube], range: Range) = {
    val input: List[List[Char]] =
      readAllLines("day-17-input.txt")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .map(_.toList)
        .toList

    val range = Range(
      xMin = 0,
      xMax = input.size - 1,
      yMin = 0,
      yMax = input(0).size - 1,
      zMin = 0,
      zMax = 0
    )

    var activeCubes = Set.empty[Cube]

    input.zipWithIndex.foreach { case (row, x) =>
      row.zipWithIndex.foreach { case (entry, y) =>
        if (entry == '#') activeCubes += Cube(x, y, 0)
      }
    }

    (activeCubes, range)
  }

  println(solutionToFirstHalf(range, activeCubes))
  val hyperRange = range.toHyperRange(0, 0)
  val activeHyperCubes = activeCubes.map(_.toHyperCube(0))
  println(solutionToSecondHalf(hyperRange, activeHyperCubes))
}
