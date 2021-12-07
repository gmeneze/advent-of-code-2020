import InputReader._
import scala.collection.mutable

object Day1 extends App {
  def solutionToFirstPart(expenses: List[Int], total: Int): Option[Long] = {
    val seenExpenses = mutable.Set.empty[Int]

    def targetExpense(exp: Int): Int = total - exp

    def wasTargetExpSeen(exp: Int): Boolean = {
      val wasSeen = seenExpenses.contains(targetExpense(exp))
      seenExpenses.addOne(exp)
      wasSeen
    }

    expenses.collectFirst {
      case expense if wasTargetExpSeen(expense) =>
        expense.toLong * targetExpense(expense).toLong
    }
  }

  def solutionToSecondPart(expenses: List[Int]): Option[Long] = {
    object CustomMatcher {
      def unapply(expenseAndIdx: Tuple2[Int, Int]): Option[Long] = {
        val (expense, idx) = expenseAndIdx
        val restOfExpenses = expenses.take(idx) ++ expenses.drop(idx + 1)
        val remainingTotal = 2020 - expense
        solutionToFirstPart(restOfExpenses, remainingTotal).map(_ * expense)
      }
    }

    expenses.zipWithIndex.collectFirst { case CustomMatcher(result) => result }
  }

  val inputExpenses =
    readAllLines("day-1-input.txt").map(_.strip).map(_.toInt)

  println(solutionToFirstPart(inputExpenses, 2020).get)
  println(solutionToSecondPart(inputExpenses).get)
}
