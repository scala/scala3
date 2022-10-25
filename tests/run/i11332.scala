// scalajs: --skip
import scala.language.unsafeNulls

import java.lang.invoke._, MethodType.methodType

object Test extends O:
  def main(args: Array[String]): Unit = ()

class O {
  def bar(x: Int): Int = -x
  def baz(s: String): String = s.reverse
  def dingo(l: Long): String = "long"
  def dingo(i: Int): String = "int"
  def putStr(s: String): Unit = ()
  def returnAny(s: String): Object = s
  def id[T](x: T): T = x

  val l = MethodHandles.lookup()
  val mh1 = l.findVirtual(classOf[O], "bar", methodType(classOf[Int], classOf[Int]))
  val mh2 = l.findVirtual(classOf[O], "baz", methodType(classOf[String], classOf[String]))
  val mh3 = l.findVirtual(classOf[O], "dingo", methodType(classOf[String], classOf[Long]))
  val mh4 = l.findVirtual(classOf[O], "dingo", methodType(classOf[String], classOf[Int]))
  val mh6 = l.findVirtual(classOf[O], "putStr", methodType(classOf[Unit], classOf[String]))
  val mh7 = l.findVirtual(classOf[O], "returnAny", methodType(classOf[Any], classOf[String]))
  val mh8 = l.findStatic(classOf[ClassLoader], "getPlatformClassLoader", methodType(classOf[ClassLoader]))

  val test1_1 = assert(-42 == (mh1.invokeExact(this, 42): Int))
  val test1_2 = assert(-33 == (mh1.invokeExact(this, 33): Int))

  val test2_1 = assert("oof" == (mh2.invokeExact(this, "foo"): String))
  val test2_2 = assert("rab" == (mh2.invokeExact(this, "bar"): String))
  val test3   = assert("long" == (mh3.invokeExact(this, 1L): String))
  val test4   = assert("int" == (mh4.invokeExact(this, 1): String))
  val test5_1 = assert(-3 == (id(mh1.invokeExact(this, 3)): Int))
  val test5_2 = expectWrongMethod(mh1.invokeExact(this, 4))

  val test6_1 = mh6.invokeExact(this, "hi"): Unit
  val test6_2 = { val hi2: Unit = mh6.invokeExact(this, "hi2"); assert((()) == hi2) }
  val test6_3 = { def hi3: Unit = mh6.invokeExact(this, "hi3"); assert((()) == hi3) }

  val test7_statbk = { mh7.invokeExact(this, "any"); () }
  val test7_valdef = { val any2 = mh7.invokeExact(this, "any2"); assert("any2" == any2) }
  val test7_defdef = { def any3 = mh7.invokeExact(this, "any3"); assert("any3" == any3) }

  val cl1: ClassLoader = mh8.invoke()
  val cl2: ClassLoader = mh8.invoke().asInstanceOf[ClassLoader]

  val test9_1 = expectWrongMethod(assert(-1 == mh1.invokeExact(this, 1)))
  val test9_3 = expectWrongMethod {
    l.findVirtual(classOf[O], "bar", methodType(classOf[Int], classOf[Int])).invokeExact(this, 3) // testing inline
  }
  val test9_4 =
    val res = l
      .findVirtual(classOf[O], "bar", methodType(classOf[Int], classOf[Int])) // testing inline
      .invokeExact(this, 4): Int
    assert(-4 == res)

  def expectWrongMethod(op: => Any) =
    try
      op
      throw new AssertionError("expected operation to fail but it didn't")
    catch case expected: WrongMethodTypeException => ()
}
