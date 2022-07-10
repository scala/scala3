// scalajs: --skip

import scala.annotation.newMain

// Sample main method
object myProgram:

  @newMain def shortFlags(a: Boolean, b: Boolean): Unit =
    println(s"shortFlags: a = $a, b = $b")

  @newMain def longFlags(flag1: Boolean, flag2: Boolean): Unit =
    println(s"longFlags: flag1 = $flag1, flag2 = $flag2")

  @newMain def mixedFlags(a: Boolean, flag: Boolean): Unit =
    println(s"mixedFlags: a = $a, flag = $flag")

end myProgram

object Test:
  def callMain(name: String, args: String*): Unit =
    val clazz = Class.forName(name)
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args.toArray)

  def main(args: Array[String]): Unit =
    callMain("shortFlags")
    callMain("shortFlags", "-a")
    callMain("shortFlags", "-a", "-b")
    callMain("shortFlags", "true", "false")
    callMain("shortFlags", "-a", "true")
    callMain("shortFlags", "-b", "true")


    callMain("longFlags")
    callMain("longFlags", "--flag1")
    callMain("longFlags", "--flag1", "--flag2")

    callMain("mixedFlags")
    callMain("mixedFlags", "-a")
    callMain("mixedFlags", "-a", "--flag")


end Test
