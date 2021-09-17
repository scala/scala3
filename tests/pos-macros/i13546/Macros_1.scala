package mylib
import scala.quoted.*

object Main:
  protected def foo: Unit = {}
  inline def fooCaller: Unit =
    def f = foo
    foo
  inline def fooCallerM: Unit = ${ fooMacro }
  def fooMacro(using Quotes): Expr[Unit] =
    '{ foo }
    val fooExpr = '{ foo }
    '{ $fooExpr }
