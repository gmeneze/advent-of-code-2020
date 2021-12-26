import InputReader._

import scala.collection.mutable

object Day18 extends App {
  sealed trait Input
  case class Number(value: Long) extends Input
  sealed trait Operator extends Input
  case object Add extends Operator
  case object Multiply extends Operator
  case object LeftParen extends Operator
  case object RightParen extends Operator
  case class Expression(value: List[Input])

  object Expression {
    def fromString(str: String): Expression = {
      var expression = List.empty[Input]
      var remainingStr = str
      while (!remainingStr.isEmpty) {
        remainingStr = remainingStr.dropWhile(_.isWhitespace)

        if (remainingStr.head.isDigit) {
          val (numStr: String, remStr: String) = remainingStr.span(_.isDigit)
          remainingStr = remStr
          expression = expression.appended(Number(numStr.toLong))
        } else if (remainingStr.head == '+') {
          remainingStr = remainingStr.tail
          expression = expression.appended(Add)
        } else if (remainingStr.head == '*') {
          remainingStr = remainingStr.tail
          expression = expression.appended(Multiply)
        } else if (remainingStr.head == '(') {
          remainingStr = remainingStr.tail
          expression = expression.appended(LeftParen)
        } else if (remainingStr.head == ')') {
          remainingStr = remainingStr.tail
          expression = expression.appended(RightParen)
        } else {
          throw new IllegalArgumentException(s"$remainingStr cannot be decoded")
        }
      }
      Expression(expression)
    }
  }

  def evaluateExpression(expression: Expression): Long = {
    var expr = expression.value

    def compute: Long = {
      var operatorStack = mutable.Stack.empty[Operator]
      var operandStack = mutable.Stack.empty[Number]
      while (!expr.isEmpty) {
        expr.head match {
          case num: Number =>
            if (!operatorStack.isEmpty) {
              val firstOperand = operandStack.pop
              val secondOperand = num
              val operator = operatorStack.pop
              val result: Long =
                operator match {
                  case Add      => firstOperand.value + secondOperand.value
                  case Multiply => firstOperand.value * secondOperand.value
                }
              operandStack.push(Number(result))
            } else {
              operandStack.push(num)
            }
            expr = expr.tail
          case operator: Operator =>
            operator match {
              case Add =>
                operatorStack = operatorStack.prepended(Add)
                expr = expr.tail
              case Multiply =>
                operatorStack = operatorStack.prepended(Multiply)
                expr = expr.tail
              case LeftParen =>
                expr = expr.tail
                val evaluatedExpr = Number(compute)
                expr = expr.prepended(evaluatedExpr)
              case RightParen =>
                expr = expr.tail
                assert(
                  operandStack.size == 1,
                  s"operand stack is incorrect: $operandStack"
                )
                return operandStack.pop().value
            }

        }

      }
      assert(
        operandStack.size == 1,
        s"operand stack is incorrect: $operandStack"
      )
      assert(
        operatorStack.isEmpty,
        s"operator stack is incorrect: $operatorStack"
      )
      operandStack.pop().value
    }

    compute
  }

  def evaluateExpressionV2(expression: Expression): Long = {
    var expr = expression.value

    def compute: Long = {
      var operatorStack = mutable.Stack.empty[Operator]
      var operandStack = mutable.Stack.empty[Number]
      while (!expr.isEmpty && expr.head != RightParen) {
        expr.head match {
          case num: Number =>
            // evaluate addition right away, mult to be done at the end
            if (!operatorStack.isEmpty && operatorStack.head == Add) {
              val firstOperand = operandStack.pop
              val secondOperand = num
              operatorStack.pop
              val result: Long = firstOperand.value + secondOperand.value
              operandStack.push(Number(result))
            } else {
              operandStack.push(num)
            }
            expr = expr.tail
          case operator: Operator =>
            operator match {
              case Add =>
                operatorStack = operatorStack.prepended(Add)
                expr = expr.tail
              case Multiply =>
                operatorStack = operatorStack.prepended(Multiply)
                expr = expr.tail
              case LeftParen =>
                expr = expr.tail
                val evaluatedExpr = Number(compute)
                expr = expr.prepended(evaluatedExpr)
            }
        }

      }

      if (!expr.isEmpty && expr.head == RightParen) {
        expr = expr.tail
      }

      assert(
        operatorStack.forall(_ == Multiply),
        s"operator stack is incorrect: $operatorStack"
      )
      assert(
        operandStack.size == operatorStack.size + 1,
        s"operand stack is incorrect: $operandStack"
      )

      while (!operatorStack.isEmpty) {
        val secondOperand = operandStack.pop
        val firstOperand = operandStack.pop
        operatorStack.pop
        val result = secondOperand.value * firstOperand.value
        operandStack.push(Number(result))
      }

      assert(
        operandStack.size == 1,
        s"operand stack is incorrect: $operandStack"
      )
      operandStack.pop.value
    }

    compute
  }

  def solutionToFirstHalf(inputExpression: List[Expression]): Long = {
    inputExpression.foldLeft(0L) { case (acc, expr) =>
      acc + evaluateExpression(expr)
    }
  }

  def solutionToSecondHalf(inputExpression: List[Expression]): Long = {
    inputExpression.foldLeft(0L) { case (acc, expr) =>
      acc + evaluateExpressionV2(expr)
    }
  }

  val inputExpressions: List[Expression] =
    readAllLines("day-18-input.txt")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .map(Expression.fromString)

  println(solutionToFirstHalf(inputExpressions))
  println(solutionToSecondHalf(inputExpressions))

}
