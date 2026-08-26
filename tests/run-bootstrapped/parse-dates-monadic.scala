//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*

extension [A, E](x: A ? E)

  def map[B](f: A => B): B ? E = x match
    case Ok(y) => Ok(f(y))
    case Err(e) => Err(e)

  def flatMap[B](f: A => B ? E): B ? E = x match
    case Ok(y) => f(y)
    case Err(e) => Err(e)

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
/*
def parseDate3(str: String): Date ? String =
  str.split("/") match
    case Array(d, m, y) =>
      maybe:
        val day   = d.parseInt.withErr(s"malformed day: $d")?
        val month = m.parseInt.withErr(s"malformed month: $m")?
        val year  = y.parseInt.withErr(s"malformed year: $y")?
        if 1 <= day && day <= 31 else s"day $day outside allowed range 1..31"
        if 1 <= month && month <= 12 else s"month $month outside allowed range 1..12"
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
        if 1 <= day && day <= 31
        if 1 <= month && month <= 12
        Date(day, month, year)
    case _ =>
      null
*/
@main def Test =
  println("1/1/2000")
  println(parseDate("1/1/2000"))
  println(parseDate2("1/1/2000"))
  //println(parseDate3("1/1/2000"))
  //println(parseDate4("1/1/2000"))

  println("1/1-2000")
  println(parseDate("1/1-2000"))
  println(parseDate2("1/1-2000"))
  //println(parseDate3("1/1-2000"))
  //println(parseDate4("1/1-2000"))

  println("1/jan/2000")
  println(parseDate("1/jan/2000"))
  println(parseDate2("1/jan/2000"))
  //println(parseDate3("1/jan/2000"))
  //println(parseDate4("1/jan/2000"))

  println("1/13/2000")
  println(parseDate("1/13/2000"))
  println(parseDate2("1/13/2000"))
  //println(parseDate3("1/13/2000"))
  //println(parseDate4("1/13/2000"))
