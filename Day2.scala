import InputReader._

import scala.util.{Try, Success, Failure}

object Day2 extends App {
  case class PasswordPolicy(char: Char, minCount: Int, maxCount: Int)
  object PasswordPolicy {
    def fromString(s: String): Try[PasswordPolicy] = {
      s.split(" ").toList match {
        case occurences :: char :: Nil =>
          occurences.split("-").toList match {
            case minCount :: maxCount :: Nil =>
              Success(PasswordPolicy(char.head, minCount.toInt, maxCount.toInt))
            case _ =>
              Failure(new IllegalArgumentException(s"invalid policy: $s"))
          }
        case _ => Failure(new IllegalArgumentException(s"invalid policy: $s"))
      }
    }
  }

  case class Password(value: String)
  case class PolicyAndPassword(policy: PasswordPolicy, password: Password)

  object PolicyAndPassword {
    def fromString(s: String): Try[PolicyAndPassword] = {
      s.split(":").toList match {
        case policyStr :: passwordStr :: Nil =>
          for {
            policy <- PasswordPolicy.fromString(policyStr)
            password <- Success(Password(passwordStr.strip))
          } yield PolicyAndPassword(policy, password)
        case _ =>
          Failure(
            new IllegalArgumentException(
              s"string: $s cannot be decoded into password and policy"
            )
          )
      }
    }
  }

  def solutionToFirstHalf(policyPasswords: List[PolicyAndPassword]): Int = {
    policyPasswords.count { case PolicyAndPassword(policy, password) =>
      val passChars: List[Char] = password.value.toList
      val policyCharCount: Int = passChars.count(_ == policy.char)
      policy.minCount <= policyCharCount && policyCharCount <= policy.maxCount
    }
  }

  def solutionToSecondHalf(policyPasswords: List[PolicyAndPassword]): Int = {
    def zeroBasedPos(pos: Int) = pos - 1
    policyPasswords.count {
      case PolicyAndPassword(
            PasswordPolicy(char, firstPos, secondPos),
            password
          ) =>
        val passChars: Vector[Char] = password.value.toVector
        val firstPosCheck: Boolean = passChars(zeroBasedPos(firstPos)) == char
        val secondPosCheck: Boolean = passChars(zeroBasedPos(secondPos)) == char
        (firstPosCheck ^ secondPosCheck)
    }
  }

  val inputPolicyPasswords: List[PolicyAndPassword] =
    readAllLines("day-2-input.txt")
      .map(_.strip)
      .filterNot(_.isEmpty)
      .map(PolicyAndPassword.fromString(_).get)

  println(solutionToFirstHalf(inputPolicyPasswords))
  println(solutionToSecondHalf(inputPolicyPasswords))
}
