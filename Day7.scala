import InputReader._

import scala.util.Try
import scala.collection.mutable

object Day7 extends App {
  case class Bag(color: String)
  case class OuterBag(bag: Bag)
  object OuterBag {
    def fromString(str: String): Try[OuterBag] = Try {
      val colorPattern = """(.+) bags""".r
      str match {
        case colorPattern(color) => OuterBag(Bag(color))
        case _ => throw new IllegalArgumentException(s"invalid bag: $str")
      }
    }
  }
  case class InnerBag(bag: Bag, quantity: Int)
  object InnerBag {
    def fromString(str: String): Try[InnerBag] = Try {
      val pattern = """(\d+) (.+) bag.*""".r
      str match {
        case pattern(quantity, color) => InnerBag(Bag(color), quantity.toInt)
        case _ => throw new IllegalArgumentException(s"invalid bag: $str")
      }
    }
  }

  def solutionToFirstHalf: Int = {
    def pathExists(
        srcBag: Bag,
        destBag: Bag
    ): Boolean = {
      if (srcBag == destBag) return true

      val innerBags: List[Bag] = ADJACENCY_LIST(OuterBag(srcBag)).map(_.bag)

      for (bag <- innerBags) {
        if (pathExists(bag, destBag)) return true
      }
      return false
    }

    val shinyGoldBag: Bag = Bag("shiny gold")

    ADJACENCY_LIST.keys.foldLeft(0) { case (acc, OuterBag(bag)) =>
      if (bag == shinyGoldBag) acc + 0
      else if (pathExists(bag, shinyGoldBag)) acc + 1
      else acc + 0
    }
  }

  def solutionToSecondHalf: Int = {
    def countBags(bag: Bag): Int = {
      val innerBags: List[InnerBag] = ADJACENCY_LIST(OuterBag(bag))

      innerBags.map { innerBag =>
        val innerCount: Int = innerBag.quantity * countBags(innerBag.bag)
        innerBag.quantity + innerCount
      }.sum
    }

    val shinyGoldBag: Bag = Bag("shiny gold")

    countBags(shinyGoldBag)
  }

  val ADJACENCY_LIST: Map[OuterBag, List[InnerBag]] = {
    def parseLine(line: String): (OuterBag, List[InnerBag]) = {
      val Array(outerBagStr, innerBagsStr) = line.split(" contain ")

      val outerBag: OuterBag = OuterBag.fromString(outerBagStr.trim).get

      val innerBags: List[InnerBag] = {
        if (innerBagsStr.contains("no other bags")) List.empty[InnerBag]
        else
          innerBagsStr
            .split(",")
            .map(_.trim)
            .map(s => InnerBag.fromString(s).get)
            .toList
      }

      outerBag -> innerBags
    }

    readAllLines("day-7-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(parseLine)
      .toMap
  }

  println(solutionToFirstHalf)
  println(solutionToSecondHalf)
}
