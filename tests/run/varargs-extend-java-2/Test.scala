// scalajs: --skip

import base.*

object Test extends App {
  class Concrete extends AbstractBase {
     override def doStuff(params: AnyRef*): Unit = println("doStuff invoked")
  }

  val impl = Concrete()
  impl.doStuff(null)

  val caller = Caller()
  caller.callDoStuff(impl)
}
