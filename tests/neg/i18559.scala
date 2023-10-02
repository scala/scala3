//> using options -Werror -Wall

val b = // OK
  var e3 = 2 // error
  1

object FooUnused:
  import collection.mutable.Set // error
  import collection.mutable.{Map => MutMap} // error
  import collection.mutable._ // error

object FooWildcardUnused:
  import collection.mutable._ // error

object Foo:
  import collection.mutable.Set // OK
  import collection.mutable.{Map => MutMap} // OK

  val bar = Set() // OK
  val baz = MutMap() // OK

sealed trait Calc
sealed trait Const extends Calc
case class Sum(a: Calc, b: Calc) extends Calc
case class S(pred: Const) extends Const
case object Z extends Const

val a = Sum(S(S(Z)),Z) match {
  case Sum(a,Z) => Z // error
  // case Sum(a @ _,Z) => Z // todo : this should pass in the future
  case Sum(a@S(_),Z) => Z // error
  case Sum(a@S(_),Z) => a // OK
  case Sum(a@S(b@S(_)), Z) => a // error
  case Sum(a@S(b@S(_)), Z) => a // error
  case Sum(a@S(b@(S(_))), Z) => Sum(a,b) // OK
  case Sum(_,_) => Z // OK
  case _ => Z // OK
}

import scala.util.{Either, Right, Left}
import scala.collection.mutable

case class Failed(msg: String)

def firstThing(): Either[Failed, Unit] =
  Right(())

def secondThing(): Either[Failed, Unit] =
  Left(Failed("whoops you should have flatMapped me"))

def singleExpr(): Either[Failed, Unit] =
  firstThing().map(_ => secondThing()) // error

def block(): Either[Failed, Unit] = {
  firstThing().map(_ => secondThing()) // error 
}

class C {
  import concurrent._
  import ExecutionContext.Implicits._
  def c = {
    def improved = Future(42)
    def stale = Future(27)
    improved  // error
    stale
  }
}