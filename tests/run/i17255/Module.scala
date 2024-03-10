// scalajs: --skip

package p {
  object Module {
    override def toString = "Module"
  }
}

object Test extends App {
  assert(p.J.f().toString == "J")
  assert(p.J.module().toString == "Module")
  assert(p.J.module2().toString == "Module")
}