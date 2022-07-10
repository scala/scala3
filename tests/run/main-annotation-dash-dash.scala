// scalajs: --skip

import scala.annotation.newMain

object myProgram:

  @newMain def foo(str: String, rest: String*): Unit =
    println(s"str = $str")
    println(s"rest = ${rest.mkString(",")}")
    println()

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("foo")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("x", "y", "z"))
    callMain(Array("--", "x", "y", "z"))
    callMain(Array("--", "-a", "x", "y", "z"))
    callMain(Array("x", "--", "y", "z"))
    callMain(Array("--str", "y", "--", "z"))
    callMain(Array("--str", "--", "y", "z")) // missing argument for `--str`
end Test
