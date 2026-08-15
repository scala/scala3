//> using options -Yexplicit-nulls
package scala.util
import boundary.{Label, break}
import language.experimental.magic

infix type `??`[+R, +E] = Result[R, E]

inline def maybe[R, E](inline body: Label[Err[E]] ?=> R): R ?? E =
  boundary(Ok(body))

implicit def toResult[R, E](x: R): R ?? E = Ok(x)

def NULL: Err[Unit] = Err(())

extension (str: String) def parseInt: Int ?? Unit =
  try str.toInt
  catch case ex: NumberFormatException => Err(null)

case class Date(day: Int, month: Int, year: Int)

def parseDate(str: String) =
  str.split("/") match
    case Array(d, m, y) =>
      maybe:
        Date(d.parseInt?, m.parseInt?, y.parseInt?)
    case _ =>
      NULL

extension [R, D](x: R ?? D)
  def withErr[E](msg: E): R ?? E = x match
    case Ok(y) => Ok(y)
    case Err(_) => Err(msg)

def parseDate2(str: String): Date ?? String =
  str.split("/") match
    case Array(d, m, y) =>
      maybe:
        val day   = d.parseInt.withErr(s"malformed day: $d")?
        val month = m.parseInt.withErr(s"malformed month: $m")?
        val year  = y.parseInt.withErr(s"malformed year: $y")?
        Date(day, month, year)
    case _ =>
      Err("Date not in format day/month/year")

def provided(cond: Boolean)(using Label[Err[Unit]]): Unit =
  if !cond then boundary.break(NULL)

inline def provided[E](cond: Boolean, inline err: E)(using Label[Err[E]]): Unit =
  if !cond then boundary.break(Err(err))

def parseDate3(str: String): Date ?? String =
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

def parseDate4(str: String): Date ?? Unit =
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
      NULL
