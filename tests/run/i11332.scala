// scalajs: --skip
import scala.language.unsafeNulls

import java.lang.invoke._, MethodType.methodType

object Test extends Foo:
  def main(args: Array[String]): Unit = ()

class Foo {
  def neg(x: Int): Int = -x
  def rev(s: String): String = s.reverse
  def over(l: Long): String = "long"
  def over(i: Int): String  = "int"
  def unit(s: String): Unit = ()
  def obj(s: String): Object = s
  def id[T](x: T): T = x

  val l = MethodHandles.lookup()
  val mhNeg   = l.findVirtual(classOf[Foo], "neg", methodType(classOf[Int], classOf[Int]))
  val mhRev   = l.findVirtual(classOf[Foo], "rev", methodType(classOf[String], classOf[String]))
  val mhOverL = l.findVirtual(classOf[Foo], "over", methodType(classOf[String], classOf[Long]))
  val mhOverI = l.findVirtual(classOf[Foo], "over", methodType(classOf[String], classOf[Int]))
  val mhUnit  = l.findVirtual(classOf[Foo], "unit", methodType(classOf[Unit], classOf[String]))
  val mhObj   = l.findVirtual(classOf[Foo], "obj", methodType(classOf[Any], classOf[String]))
  val mhCL    = l.findStatic(classOf[ClassLoader], "getPlatformClassLoader", methodType(classOf[ClassLoader]))

  val testNeg1 = assert(-42 == (mhNeg.invokeExact(this, 42): Int))
  val testNeg2 = assert(-33 == (mhNeg.invokeExact(this, 33): Int))

  val testRev1  = assert("oof" == (mhRev.invokeExact(this, "foo"): String))
  val testRev2  = assert("rab" == (mhRev.invokeExact(this, "bar"): String))

  val testOverL = assert("long" == (mhOverL.invokeExact(this, 1L): String))
  val testOVerI = assert("int" == (mhOverI.invokeExact(this, 1): String))

  val testNeg_tvar = assert(-3 == (id(mhNeg.invokeExact(this, 3)): Int))
  val testNeg_obj  = expectWrongMethod(mhNeg.invokeExact(this, 4))

  val testUnit_exp = { mhUnit.invokeExact(this, "hi"): Unit; () }
  val testUnit_val = { val hi2: Unit = mhUnit.invokeExact(this, "hi2"); assert((()) == hi2) }
  val testUnit_def = { def hi3: Unit = mhUnit.invokeExact(this, "hi3"); assert((()) == hi3) }

  val testObj_exp = { mhObj.invokeExact(this, "any"); () }
  val testObj_val = { val any2 = mhObj.invokeExact(this, "any2"); assert("any2" == any2) }
  val testObj_def = { def any3 = mhObj.invokeExact(this, "any3"); assert("any3" == any3) }

  val testCl1_pass = assert(null != (mhCL.invoke(): ClassLoader))
  val testCl2_cast = assert(null != (mhCL.invoke().asInstanceOf[ClassLoader]: ClassLoader))

  val testNeg_inline_obj = expectWrongMethod(l
    .findVirtual(classOf[Foo], "neg", methodType(classOf[Int], classOf[Int]))
    .invokeExact(this, 3))
  val testNeg_inline_pass = assert(-4 == (l
    .findVirtual(classOf[Foo], "neg", methodType(classOf[Int], classOf[Int]))
    .invokeExact(this, 4): Int))

  def expectWrongMethod(op: => Any) = try {
    op
    throw new AssertionError("expected operation to fail but it didn't")
  } catch case expected: WrongMethodTypeException => ()
}
