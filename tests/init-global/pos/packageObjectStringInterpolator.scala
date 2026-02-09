package p
package object a {
  val b = 10
  implicit class CI(s: StringContext) {
    def ci(args: Any*) = 10
  }
}

import p.a._

object A:
  val f = b // p.a(ObjectRef(p.a)).b
  def foo(s: String): String = s
  val f1 = ci"a" // => p.a(Package(p).select(a)).CI(StringContext"a").ci()


