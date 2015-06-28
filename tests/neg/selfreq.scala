trait X { self: Y =>

  type T <: self.U

  def foo(x: T): T
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
  x.foo("a")
}

import scala.tools.nsc.interpreter.IMain

object Test extends dotty.runtime.LegacyApp {
  val engine = new IMain.Factory getScriptEngine()
  engine.asInstanceOf[IMain].settings.usejavacp.value = true
  val res2 = engine.asInstanceOf[javax.script.Compilable]
  res2 compile "8" eval()
  val res5 = res2 compile """println("hello") ; 8"""
  res5 eval()
  res5 eval()
}
