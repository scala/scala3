// scalajs: --skip
import scala.language.unsafeNulls

import java.lang.invoke._, MethodType.methodType

// A copy of tests/run/i11332.scala
// to test the bootstrap minimisation which failed
// (because bootstrap runs under explicit nulls)
class Foo:
  def neg(x: Int): Int = -x

  val l    = MethodHandles.lookup()
  val self = new Foo()

  val test = // testing as a expression tree - previously derivedSelect broke the type
    l
      .findVirtual(classOf[Foo], "neg", methodType(classOf[Int], classOf[Int]))
      .invokeExact(self, 4): Int
