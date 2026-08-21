//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*

extension (str: String) def parseInt: Int ? Unit =
  try str.toInt
  catch case ex: NumberFormatException => null

case class Date(day: Int, month: Int, year: Int)

def parseDate(str: String) =
  str.split("/") match
    case Array(d, m, y) =>
      maybe:
        Date(d.parseInt?, m.parseInt?, y.parseInt?)
    case _ =>
      null

def parseDate2(str: String): Date ? String =
  str.split("/") match
    case Array(d, m, y) =>
      maybe:
        val day   = d.parseInt.withErr(s"malformed day: $d")?
        val month = m.parseInt.withErr(s"malformed month: $m")?
        val year  = y.parseInt.withErr(s"malformed year: $y")?
        Date(day, month, year)
    case _ =>
      Err("Date not in format day/month/year")

def parseDate3(str: String): Date ? String =
  str.split("/") match
    case Array(d, m, y) =>
      maybe:
        val day   = d.parseInt.withErr(s"malformed day: $d")?
        val month = m.parseInt.withErr(s"malformed month: $m")?
        val year  = y.parseInt.withErr(s"malformed year: $y")?
        provided(1 <= day && day <= 31, s"day $day outside allowed range 1..31")
        provided(1 <= month && month <= 12, s"month $month outside allowed range 1..12")
        Date(day, month, year)
    case _ =>
      Err("Date not in format day/month/year")

def parseDate4(str: String): Date? =
  str.split("/") match
    case Array(d, m, y) =>
      maybe:
        val day   = d.parseInt?
        val month = m.parseInt?
        val year  = y.parseInt?
        provided(1 <= day && day <= 31)
        provided(1 <= month && month <= 12)
        Date(day, month, year)
    case _ =>
      null
