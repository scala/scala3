// scalajs: --skip

import scala.annotation.newMain

object myProgram:

  /** Adds two numbers */
  @newMain def add(n: Int, i: Int): Unit =
    println(s"$n + $i = ${n + i}")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("-n", "2", "-i", "3"))
    callMain(Array("-i", "3", "-n", "2"))

    callMain(Array("--n", "2", "--i", "3"))
end Test
