import InputReader._

import scala.util.matching.Regex

object Day16 extends App {
  case class Range(start: Int, end: Int)
  case class Rule(name: String, range1: Range, range2: Range) {
    def satisfied(num: Int): Boolean = {
      val Range(s1, e1) = range1
      val Range(s2, e2) = range2

      (num >= s1 && num <= e1) || (num >= s2 && num <= e2)
    }

    def nameContainsString(str: String): Boolean =
      name.contains(str)
  }

  case class Ticket(fields: List[Int]) {
    override def toString: String =
      fields.mkString(",")
  }

  object Rule {
    def fromString(str: String): Rule = {
      val pattern: Regex = """(.*): (\d+)-(\d+) or (\d+)-(\d+)""".r
      str match {
        case pattern(name, s1, e1, s2, e2) =>
          Rule(name, Range(s1.toInt, e1.toInt), Range(s2.toInt, e2.toInt))
      }
    }
  }

  def solutionToFirstHalf(
      rules: List[Rule],
      nearbyTickets: List[Ticket]
  ): Long = {
    val invalidTicketFields: List[List[Int]] =
      nearbyTickets.map { ticket =>
        ticket.fields.filterNot { field =>
          val fieldSatisfiesAnyRule: Boolean =
            rules.exists(rule => rule.satisfied(field))

          fieldSatisfiesAnyRule
        }
      }

    val invalidTicketSums: List[Int] =
      invalidTicketFields.map(_.sum)

    invalidTicketSums.foldLeft(0L) { case (acc, ticketSum) =>
      acc + ticketSum
    }
  }

  def solutionToSecondHalf(
      rules: List[Rule],
      myTicket: Ticket,
      nearbyTickets: List[Ticket]
  ): Long = {
    val allTickets: List[Ticket] = nearbyTickets.prepended(myTicket)

    val validTickets: List[Ticket] =
      allTickets.filter { ticket =>
        ticket.fields.forall(field =>
          rules.exists(rule => rule.satisfied(field))
        )
      }

    var ruleToLocation: Map[Rule, Int] = Map.empty[Rule, Int]
    var remainingLocations: Set[Int] = (0 until myTicket.fields.size).toSet
    var remainingRules: Set[Rule] = rules.toSet

    while (!remainingLocations.isEmpty) {
      remainingLocations = remainingLocations.filter { location =>
        var locationStillRemaining: Boolean = true

        val fieldsAtLocation: List[Int] = validTickets.map(_.fields(location))
        val satisfiedRules: Set[Rule] =
          remainingRules.filter { rule =>
            fieldsAtLocation.forall(rule.satisfied)
          }

        if (satisfiedRules.size == 1) {
          val rule = satisfiedRules.head
          ruleToLocation += (rule -> location)
          locationStillRemaining = false
          remainingRules -= rule
        }

        locationStillRemaining
      }
    }

    println(ruleToLocation)
    val relevantRules: Map[Rule, Int] = ruleToLocation.filter {
      case (rule, _) => rule.nameContainsString("departure")
    }
    val relevantLocations: List[Int] = relevantRules.values.toList

    var result: Long = 1L

    for {
      i <- relevantLocations
    } {
      result *= myTicket.fields(i)
    }

    result
  }

  val (rules: List[Rule], myTicket: Ticket, nearbyTickets: List[Ticket]) = {
    val inputSections: List[String] =
      readAll("day-16-input.txt")
        .split("\n\n")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .toList

    inputSections match {
      case rulesStr :: myTicketStr :: nearbyTicketsStr :: Nil =>
        val rules: List[Rule] = {
          val lines: List[String] =
            rulesStr.split("\n").map(_.trim).filterNot(_.isEmpty).toList
          lines.map(Rule.fromString)
        }

        val myTicket: Ticket = {
          val lines: List[String] =
            myTicketStr.split("\n").map(_.trim).filterNot(_.isEmpty).toList

          val ticketLine: String = lines.tail.head
          val ticketFields: List[Int] =
            ticketLine
              .split(",")
              .map(_.trim)
              .filterNot(_.isEmpty)
              .map(_.toInt)
              .toList

          Ticket(ticketFields)
        }

        val nearbyTickets: List[Ticket] = {
          val lines: List[String] =
            nearbyTicketsStr.split("\n").map(_.trim).filterNot(_.isEmpty).toList

          val ticketLines: List[String] = lines.tail

          val ticketFieldsList: List[List[Int]] =
            ticketLines.map { line =>
              line
                .split(",")
                .map(_.trim)
                .filterNot(_.isEmpty)
                .map(_.toInt)
                .toList
            }

          ticketFieldsList.map(Ticket.apply)
        }
        (rules, myTicket, nearbyTickets)
    }
  }

  println("rules are:")
  rules.foreach(println)

  println(s"myticket is: $myTicket")

  println("other tickets are:")
  nearbyTickets.foreach(println)

  println(solutionToFirstHalf(rules, nearbyTickets))
  println(solutionToSecondHalf(rules, myTicket, nearbyTickets))
}
