import InputReader._

import scala.collection.mutable

object Day19 extends App {
  type RuleId = Int
  sealed trait RuleRepresentation
  // List represents OR combinations
  case class RuleRef(value: List[List[RuleId]]) extends RuleRepresentation
  case class Rule(value: List[String]) extends RuleRepresentation

  def resolveRuleRefs(
      ruleMap: Map[RuleId, RuleRepresentation]
  ): Map[RuleId, Rule] = {
    var newRuleMap = mutable.Map.empty[RuleId, Rule]

    def resolveRule(id: RuleId): Rule = {
      if (newRuleMap contains id) return newRuleMap(id)

      def getAllRules(ruleList: List[RuleId]): List[String] = {
        val rules: List[Rule] = ruleList.map(id => resolveRule(id))

        def combinations(rules: List[Rule], currRule: String): List[String] = {
          rules match {
            case Nil => List(currRule)
            case rule :: rules =>
              val strings: List[String] = rule.value
              strings.flatMap { string =>
                combinations(rules, currRule + string)
              }
          }
        }

        combinations(rules, "")
      }

      ruleMap(id) match {
        case r: Rule =>
          newRuleMap(id) = r
        case ref: RuleRef =>
          val ruleStrings: List[String] =
            ref.value.flatMap { ruleList =>
              getAllRules(ruleList)
            }
          newRuleMap(id) = Rule(ruleStrings)
      }

      newRuleMap(id)
    }

    for (id <- ruleMap.keys) {
      resolveRule(id)
    }

    newRuleMap.toMap
  }

  def solutionToFirstHalf(
      ruleMap: Map[RuleId, Rule],
      messages: List[String]
  ): Int = {
    def satisfiesRule0(message: String): Boolean = {
      val rule0: Rule = ruleMap(0)
      rule0.value.contains(message)
    }

    messages.count(satisfiesRule0)
  }

  def solutionToSecondHalf(
      ruleMap: Map[RuleId, Rule],
      messages: List[String]
  ): Int = {
    def satisfiesRule0(message: String): Boolean = {
      def matchRule8FollowedBy11(
          message: String,
          prevRulesMatched: List[Int]
      ): Boolean = {
        if (message.isEmpty) {
          val (thirtyOnes: List[Int], fortyTwos: List[Int]) =
            prevRulesMatched.span(_ == 31)
          if (fortyTwos.contains(31)) return false
          if (fortyTwos.size < 2) return false
          if (thirtyOnes.size == 0) return false
          if (fortyTwos.size <= thirtyOnes.size) return false
          return true
        }

        def chooseRule31(message: String): Boolean = {
          var rule31Strings: List[String] = ruleMap(31).value
          var resultFound = false
          while (!rule31Strings.isEmpty && !resultFound) {
            val string = rule31Strings.head
            rule31Strings = rule31Strings.tail

            (message.indexOf(string) == 0) match {
              case true =>
                resultFound = matchRule8FollowedBy11(
                  message.substring(string.size),
                  prevRulesMatched.prepended(31)
                )
              case false => // continue
            }
          }

          resultFound
        }

        def chooseRule42(message: String): Boolean = {
          var rule42Strings: List[String] = ruleMap(42).value
          var resultFound = false
          while (!rule42Strings.isEmpty && !resultFound) {
            val string = rule42Strings.head
            rule42Strings = rule42Strings.tail

            (message.indexOf(string) == 0) match {
              case true =>
                resultFound = matchRule8FollowedBy11(
                  message.substring(string.size),
                  prevRulesMatched.prepended(42)
                )
              case false => // continue
            }
          }

          resultFound
        }

        prevRulesMatched match {
          case Nil =>
            chooseRule42(message)
          case _ =>
            chooseRule42(message) || chooseRule31(message)
        }
      }

      matchRule8FollowedBy11(message, List.empty)
    }

    val satisfiedStrings = messages.filter(satisfiesRule0)
    satisfiedStrings.foreach(println)
    satisfiedStrings.size
  }

  val (ruleRepMap: Map[RuleId, RuleRepresentation], messages: List[String]) = {
    var map = mutable.Map.empty[RuleId, RuleRepresentation]

    val (rules: List[String], messages: List[String]) = {
      val Array(rulesStr: String, messagesStr: String) =
        readAll("day-19-input.txt")
          .split("\n\n")

      val rules: List[String] =
        rulesStr.split("\n").map(_.trim).filterNot(_.isEmpty).toList

      val messages: List[String] =
        messagesStr.split("\n").map(_.trim).filterNot(_.isEmpty).toList

      (rules, messages)
    }

    def getRuleIds(str: String): List[RuleId] = {
      var rules = List.empty[RuleId]
      for (id <- str.split(" ").map(_.trim).filterNot(_.isEmpty).toList) {
        rules = rules.appended(id.toInt)
      }
      rules
    }

    for (line <- rules) {
      val Array(idStr: String, ruleStr: String) = line.split(":")
      val ruleId: RuleId = idStr.toInt
      val trimmedRuleStr = ruleStr.trim
      val isAruleRef: Boolean = ruleStr.exists(_.isDigit)
      isAruleRef match {
        case true =>
          val isAnOrRule: Boolean = trimmedRuleStr.exists(_ == '|')
          isAnOrRule match {
            case true =>
              val Array(leftRules: String, rightRules: String) =
                trimmedRuleStr.split("""\|""")
              val leftRuleIds: List[RuleId] = getRuleIds(leftRules.trim)
              val rightRuleIds: List[RuleId] = getRuleIds(rightRules.trim)
              map(ruleId) = RuleRef(List(leftRuleIds, rightRuleIds))
            case false =>
              val ruleIds: List[RuleId] = getRuleIds(trimmedRuleStr)
              map(ruleId) = RuleRef(List(ruleIds))
          }
        case false =>
          val rule: String =
            trimmedRuleStr.split("\"").map(_.trim).filterNot(_.isEmpty).head
          map(ruleId) = Rule(List(rule))
      }
    }

    (map.toMap, messages)
  }

  val ruleMap: Map[RuleId, Rule] = resolveRuleRefs(ruleRepMap)
  println(solutionToFirstHalf(ruleMap, messages))
  println(solutionToSecondHalf(ruleMap, messages))
}
