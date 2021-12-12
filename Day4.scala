import InputReader._

import scala.util.{Try, Success, Failure}

object Day4 extends App {
  type Passport = List[String]
  sealed trait PassportField
  object PassportField {
    case class BirthYear(value: Int) extends PassportField {
      require(value >= 1920 && value <= 2002, s"birth year: $value")
    }
    case class IssueYear(value: Int) extends PassportField {
      require(value >= 2010 && value <= 2020, s"issue year: $value")
    }
    case class ExpirationYear(value: Int) extends PassportField {
      require(value >= 2020 && value <= 2030, s"expiration year: $value")
    }
    case class Height(value: String) extends PassportField {
      val cmPattern = """(\d+)cm""".r
      val inPattern = """(\d+)in""".r
      value match {
        case cmPattern(heightStr) =>
          val height = heightStr.toInt
          require(height >= 150 && height <= 193, s"height: $value")
        case inPattern(heightStr) =>
          val height = heightStr.toInt
          require(height >= 59 && height <= 76, "height")
        case _ => throw new IllegalArgumentException(s"invalid height: $value")
      }
    }
    case class HairColor(value: String) extends PassportField {
      require(value.size == 7)
      val regex = """^#[0-9a-f]+""".r
      require(regex.pattern.matcher(value).matches, s"hair color: $value")
    }
    case class EyeColor(value: String) extends PassportField {
      require(
        Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains value,
        s"eye color: $value"
      )
    }
    case class PassportId(value: String) extends PassportField {
      val regex = """\d{9}""".r
      require(regex.pattern.matcher(value).matches, s"passport id: $value")
    }
    case class CountryId(value: String) extends PassportField

    def fromString(s: String): Try[PassportField] = {
      s.split(":").toList match {
        case List("byr", value) => Try(BirthYear(value.toInt))
        case List("iyr", value) => Try(IssueYear(value.toInt))
        case List("eyr", value) => Try(ExpirationYear(value.toInt))
        case List("hgt", value) => Try(Height(value))
        case List("hcl", value) => Try(HairColor(value))
        case List("ecl", value) => Try(EyeColor(value))
        case List("pid", value) => Try(PassportId(value))
        case List("cid", value) => Try(CountryId(value))
        case _ => Failure(new IllegalArgumentException(s"invalid field: $s"))
      }
    }
  }

  def isPassportValid(entries: Passport): Boolean = {
    var passportFields = List.empty[PassportField]
    entries.foreach { line =>
      val fields: List[String] =
        line.split(" ").map(_.trim).filterNot(_.isEmpty).toList

      fields.foreach { field =>
        PassportField.fromString(field) match {
          case Success(pf)    => passportFields = passportFields.appended(pf)
          case Failure(error) => return false
        }
      }
    }
    val passportFieldsWithoutCountryId = passportFields.filterNot {
      case PassportField.CountryId(_) => true
      case _                          => false
    }

    passportFieldsWithoutCountryId.size == 7
  }

  val passports: List[Passport] = {
    val entireFile = readAll("day-4-input.txt")
    val nonEmptyLines: List[String] =
      entireFile.split("\n\n").map(_.trim).filterNot(_.isEmpty).toList
    nonEmptyLines.map { lines =>
      lines.split("\n").map(_.trim).filterNot(_.isEmpty).toList
    }
  }

  println(passports.count(isPassportValid))
}
