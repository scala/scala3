//> using options -Wvalue-discard 

import scala.util.{Either, Right, Left}
import scala.collection.mutable

case class Failed(msg: String)

def firstThing(): Either[Failed, Unit] =
  Right(())

def secondThing(): Either[Failed, Unit] =
  Left(Failed("whoops you should have flatMapped me"))

def singleExpr(): Either[Failed, Unit] =
  firstThing().map(_ => secondThing()) // warn

def block(): Either[Failed, Unit] = {
  firstThing().map(_ => secondThing()) // warn
}

class ValueDiscardTest:
  val field = mutable.Set.empty[String]

  def remove(): Unit =
    // Set#remove returns a Boolean, not this.type
    // --> Warning
    mutable.Set.empty[String].remove("") // warn

  def removeAscribed(): Unit = {
    mutable.Set.empty[String].remove(""): Unit    // nowarn
  }

  def subtract(): Unit =
    // - Set#subtractOne returns this.type
    // - receiver is not a field or a local variable (not quite sure what you'd call it)
    // --> Warning
    mutable.Set.empty[String].subtractOne("") // warn

  def mutateLocalVariable(): Unit = {
    // - Set#subtractOne returns this.type
    // - receiver is a local variable
    // --> No warning
    val s: mutable.Set[String] = mutable.Set.empty[String]
    s.subtractOne("")
  }

  def mutateField(): Unit =
    // - Set#subtractOne returns this.type
    // - receiver is a local variable
    // --> No warning
    field.subtractOne("")

  def assignmentOperator(): Unit =
    // - += returns this.type
    // - receiver is not a field or a local variable
    // --> Warning
    mutable.Set.empty[String] += "" // warn

  def assignmentOperatorLocalVariable(): Unit =
    // - += returns this.type
    // - receiver is a local variable
    // --> No warning
    val s: mutable.Set[String] = mutable.Set.empty[String]
    s += ""

// see also tests/warn/21557.scala
class UnitAscription:
  import scala.concurrent.*, ExecutionContext.Implicits.given

  case class C(c: Int):
    def f(i: Int, j: Int = c) = i + j

  def f(i: Int, j: Int = 27) = i + j

  def g[A]: List[A] = Nil

  def i: Int = 42

  def `default arg is inline`: Unit =
    f(i = 42): Unit // nowarn

  def `default arg requires block`: Unit =
    C(27).f(i = 42): Unit // nowarn

  def `application requires implicit arg`: Unit =
    Future(42): Unit // nowarn

  def `application requires inferred type arg`: Unit =
    g: Unit // nowarn

  def `implicit selection from this`: Unit =
    i: Unit // nowarn

object UnitAscription:
  def g[A]: List[A] = Nil
  def `application requires inferred type arg`: Unit =
    g: Unit // nowarn UnitAscription.g
