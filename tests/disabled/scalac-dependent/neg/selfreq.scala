trait X { self: Y =>  // error: cannot resolve reference to type (Y & X)(X.this).V

  type T <: self.U

  def foo(x: T): T  // old-error: cannot resolve reference to type (Y & X)(X.this).V
  def foo(x: String): String

}

trait Y { self: Z =>

  type U <: self.V

}

trait Z {

  class V

}

object O {
  val x: X = ???
  x.foo("a") // error: cannot resolve reference to type (Y & X)(X.this).V
}

import scala.tools.nsc.interpreter.IMain

object Test extends App {
  val engine = new IMain.Factory getScriptEngine()
  engine.asInstanceOf[IMain].settings.usejavacp.value = true // no longer an error since we unpickle Scala2 inner classes with fixed syms
  val res2 = engine.asInstanceOf[javax.script.Compilable]
  res2 compile "8" eval()
  val res5 = res2 compile """println("hello") ; 8"""
  res5 eval()
  res5 eval()
}
