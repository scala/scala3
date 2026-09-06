//> using options -language:experimental.captureChecking,experimental.separationChecking
package scala.util

import language.experimental.{captureChecking, separationChecking}

import scala.util.boundary, boundary.{break, Label}

import collection.mutable
import caps.Control
import caps.fresh
import caps.any
import caps.Control

import Validation.{Checked, Validated, Tested, CanCheck}
import scala.annotation.publicInBinary
import Validation.Step

object Validation {

  type Validated[+T, E] = Result[T, List[E]]
  type Tested[+T] = Result[T, Unit]
  type CanCheck = boundary.Label[Err[Unit]]
  type Checked[+T] = CanCheck ?=> T
  type Step[+T, E] = (CanCheck, Validation[E]^) ?=> T
  def scope[E](using scope: Validation[E]^): scope.type = scope

  inline def validate[T, E](inline step: Step[T, E]): Validated[T, E] =
    given (Validation[E]^)()
    scope.result(step)

  val invalid: Err[Unit] = Err(())

}

class Validation[E] extends caps.Stateful, caps.ExclusiveCapability:
  self: Validation[E]^{any} =>

  private val errors = mutable.ListBuffer[E]()

  consume def close(): List[E] = {
    val es = errors.toList
    errors.clear()
    es
  }

  @publicInBinary
  private[Validation] update def appendOne(e: E): Unit =
    errors += e

  @publicInBinary
  private[Validation] update def appendAll(es: List[E]): Unit =
    errors ++= es

  update inline def test(cond: Boolean, inline error: E): Unit =
    if !cond then
      appendOne(error)

  update inline def require(cond: Boolean, inline error: E): Checked[Unit] = (lbl: CanCheck) ?=>
    if !cond then
      appendOne(error)
      break(Validation.invalid)

  update def test[A](cond: Result[A, E]): Tested[A] =
    cond match
      case ok: Ok[?] =>
        ok
      case Err(e) =>
        appendOne(e)
        Validation.invalid

  inline update def testStep[A](inline cond: Step[A, E]): Tested[A] =
    val scope: Validation[E]^{this} = this
    val tested = boundary[Tested[A]] { lbl ?=>
      Ok(cond(using lbl, scope))
    }
    tested

  update def testAll[A](validated: Validated[A, E]): Tested[A] =
    validated match
      case ok: Ok[?] =>
        ok
      case Err(es) =>
        appendAll(es)
        Validation.invalid

  inline consume def result[A](inline cond: Step[A, E]): Validated[A, E] =
    val validated = testStep(cond)
    val errs = close()
    validated match
      case ok @ Ok(_) if errs.isEmpty => ok
      case _ => Err(errs)
