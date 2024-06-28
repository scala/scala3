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

  // TODO IMHO we don't need to support this,
  // as it's just as easy to add a @nowarn annotation as a Unit ascription
  //def removeAscribed(): Unit = {
  //  mutable.Set.empty[String].remove(""): Unit    // nowarn
  //}

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