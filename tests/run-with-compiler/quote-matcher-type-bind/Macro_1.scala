import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

import scala.internal.quoted.Matcher._
import scala.internal.Quoted._

object Macros {

  inline def swapFandG(x: => Unit): Unit = ${impl('x)}

  private def impl(x: Expr[Unit])(implicit reflect: Reflection): Expr[Unit] = {
    x match {
      case '{ DSL.f[$t]($x) } => '{ DSL.g[$t]($x) }
      case '{ DSL.g[$t]($x) } => '{ DSL.f[$t]($x) }
      case _ => x
    }
  }

}

//
// DSL in which the user write the code
//

object DSL {
  def f[T](x: T): Unit = println("f: " + x.toString)
  def g[T](x: T): Unit = println("g: " + x.toString)
}
