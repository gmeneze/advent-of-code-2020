import InputReader._

object Day6 extends App {
  type Person = String
  type Group = List[Person]

  case class Answers(yeses: Set[Char])

  def solutionToFirstHalf(groups: List[Group]): Int = {
    def processGroup(group: Group): Answers = {
      var answerSet = Set.empty[Char]

      group.map { answers =>
        answerSet ++= answers.trim.toSet
      }

      Answers(answerSet)
    }

    groups.foldLeft(0) { (acc, group) => acc + processGroup(group).yeses.size }
  }

  def solutionToSecondHalf(groups: List[Group]): Int = {
    def processGroup(group: Group): Answers = {

      val groupAnswers: List[Answers] = group.map { answers =>
        Answers(answers.trim.toSet)
      }

      val commonAnswers: Answers =
        groupAnswers.reduce((answers1, answers2) =>
          Answers(answers1.yeses intersect answers2.yeses)
        )

      commonAnswers
    }

    groups.foldLeft(0) { (acc, group) => acc + processGroup(group).yeses.size }
  }

  val inputGroups: List[Group] = {
    val groupStrings: List[String] =
      readAll("day-6-input.txt")
        .split("\n\n")
        .map(_.trim)
        .filterNot(_.isEmpty)
        .toList
    groupStrings.map(_.split("\n").toList)
  }

  println(solutionToFirstHalf(inputGroups))
  println(solutionToSecondHalf(inputGroups))
}
