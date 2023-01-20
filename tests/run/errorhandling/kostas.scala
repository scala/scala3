package optionMockup:
  import scala.util.boundary, boundary.break
  object optional:
    transparent inline def apply[T](inline body: boundary.Label[None.type] ?=> T): Option[T] =
      boundary(Some(body))

  extension [T](r: Option[T])
    transparent inline def ? (using label: boundary.Label[None.type]): T = r match
      case Some(x) => x
      case None => break(None)

import optionMockup.*

case class Person(name: String, age: Int)

object PersonCsvParserIgnoreErrors:
  def parse(csv: Seq[String]): Seq[Person] =
    for
      line <- csv
      columns = line.split(",")
      parsed <- parseColumns(columns)
    yield
      parsed

  private def parseColumns(columns: Seq[String]): Option[Person] =
    columns match
      case Seq(name, age) => parsePerson(name, age)
      case _              => None

  private def parsePerson(name: String, age: String): Option[Person] =
    optional:
      Person(name, age.toIntOption.?)

def parseCsvIgnoreErrors() =
  println(PersonCsvParserIgnoreErrors.parse(Seq("Kostas,5", "George,invalid", "too,many,columns")).mkString("\n"))