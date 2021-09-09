package mylib
import scala.quoted.*

private[mylib] object Main:
  transparent inline def d(): Unit = ${interpMacro}
  def interpMacro(using Quotes) : Expr[Unit] = '{}

transparent inline def f(): Unit = Main.d()
