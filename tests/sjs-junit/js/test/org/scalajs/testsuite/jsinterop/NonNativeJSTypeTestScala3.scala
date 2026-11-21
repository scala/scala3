package org.scalajs.testsuite.jsinterop

import org.junit.Assert.*
import org.junit.Test

import scala.scalajs.js
import scala.scalajs.js.annotation.*

class NonNativeJSTypeTestScala3 {
  import NonNativeJSTypeTestScala3.*

  @Test
  def overloadWithVarargOfGenericType(): Unit = {
    class OverloadWithVarargOfGenericType extends js.Object {
      def overloaded(x: Int): Int = x
      def overloaded(xs: (Int, Int)*): Int = xs.size
    }

    val obj = new OverloadWithVarargOfGenericType
    assertEquals(5, obj.overloaded(5))
    assertEquals(1, obj.overloaded((5, 6)))
    assertEquals(2, obj.overloaded((1, 2), (3, 4)))
  }

  @Test
  def overloadWithVarargOfValueClass(): Unit = {
    class OverloadWithVarargOfValueClass extends js.Object {
      def overloaded(x: Int): Int = x
      def overloaded(xs: VC*): Int = xs.size
    }

    val obj = new OverloadWithVarargOfValueClass
    assertEquals(5, obj.overloaded(5))
    assertEquals(1, obj.overloaded(new VC(5)))
    assertEquals(2, obj.overloaded(new VC(5), new VC(6)))
  }

  @Test
  def overloadWithVarargOfGenericValueClass(): Unit = {
    class OverloadWithVarargOfGenericValueClass extends js.Object {
      def overloaded(x: Int): Int = x
      def overloaded(xs: GenVC[Int]*): Int = xs.size
    }

    val obj = new OverloadWithVarargOfGenericValueClass
    assertEquals(5, obj.overloaded(5))
    assertEquals(1, obj.overloaded(new GenVC(5)))
    assertEquals(2, obj.overloaded(new GenVC(5), new GenVC(6)))
  }

  @Test
  def overloadWithVarargOfOpaqueTypeAlias(): Unit = {
    import OpaqueContainer.*

    class OverloadWithVarargOfOpaqueTypeAlias extends js.Object {
      def overloaded(x: String): Int = x.toInt
      def overloaded(xs: OpaqueInt*): Int = xs.size
    }

    val obj = new OverloadWithVarargOfOpaqueTypeAlias
    assertEquals(5, obj.overloaded("5"))
    assertEquals(1, obj.overloaded(fromInt(5)))
    assertEquals(2, obj.overloaded(fromInt(5), fromInt(6)))
  }
}

object NonNativeJSTypeTestScala3 {
  final class VC(val x: Int) extends AnyVal

  final class GenVC[T](val x: T) extends AnyVal

  object OpaqueContainer {
    opaque type OpaqueInt = Int

    def fromInt(x: Int): OpaqueInt = x
    def toInt(x: OpaqueInt): Int = x
  }
}
