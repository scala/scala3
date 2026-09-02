//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.{Ok, Err}

extension (str: String) def parseInt: Int? =
  try str.toInt
  catch case ex: NumberFormatException => null

case class Date(day: Int, month: Int, year: Int)

def parseDate(str: String): Date? =
  str.split("/") match
    case Array(d, m, y) =>
      for
        day <- d.parseInt
        month <- m.parseInt
        year <- y.parseInt
      yield
        Date(day, month, year)
    case _ =>
      null

def parseDate2(str: String): Date ? String =
  str.split("/") match
    case Array(d, m, y) =>
      for
        day <- d.parseInt.withErr(s"malformed day: $d")
        month <- m.parseInt.withErr(s"malformed month: $m")
        year <- y.parseInt.withErr(s"malformed year: $y")
      yield
        Date(day, month, year)
    case _ =>
      Err("Date not in format day/month/year")

def parseDate4(str: String): Date? =
  str.split("/") match
    case Array(d, m, y) =>
      for
        day <- d.parseInt
        if 1 <= day && day <= 31
        month <- m.parseInt
        year <- y.parseInt
        if 1 <= month && month <= 12
      yield
        Date(day, month, year)
    case _ =>
      null

def printDate4(str: String): Unit =
  str.split("/") match
    case Array(d, m, y) =>
      for
        day <- d.parseInt
        if 1 <= day && day <= 31
        month <- m.parseInt
        year <- y.parseInt
        if 1 <= month && month <= 12
      do
        println(Date(day, month, year))
    case _ =>
      println(s"malformed date: $str")

@main def Test =
  println("1/1/2000:")
  println(parseDate("1/1/2000"))
  println(parseDate2("1/1/2000"))
  println(parseDate4("1/1/2000"))
  printDate4("1/1/2000")

  println("1/1-2000:")
  println(parseDate("1/1-2000"))
  println(parseDate2("1/1-2000"))
  println(parseDate4("1/1-2000"))
  printDate4("1/1-2000")

  println("1/jan/2000:")
  println(parseDate("1/jan/2000"))
  println(parseDate2("1/jan/2000"))
  println(parseDate4("1/jan/2000"))
  printDate4("1/jan/2000")

  println("1/13/2000:")
  println(parseDate("1/13/2000"))
  println(parseDate2("1/13/2000"))
  println(parseDate4("1/13/2000"))
  printDate4("1/13/2000")
