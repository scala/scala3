//> using options -source future
// scalajs: --skip

import java.lang.invoke.*

object Test:
  def main(args: Array[String]): Unit =
    val mh = MethodHandles.identity(classOf[String])
    val a: String = mh.invokeExact("a"): String
    assert(a == "a")
    // the cast does not affect the call's signature, which is (String)Object
    try
      val b: String = mh.invokeExact("b").asInstanceOf[String] // warn
      throw AssertionError("expected WrongMethodTypeException")
    catch case _: WrongMethodTypeException => ()
