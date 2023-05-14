// scalajs: --skip

import scala.annotation.newMain
import scala.annotation.newMain.alias

object myProgram:
  @newMain def multipleSameShortNames1(
    @alias("n") num: Int,
    @alias("n") inc: Int
  ): Unit = ()

  @newMain def multipleSameShortNames2(
    @alias("n") @alias("n") num: Int,
    inc: Int
  ): Unit = ()

  @newMain def multipleSameNames1(
    @alias("arg") num: Int,
    @alias("arg") inc: Int
  ): Unit = ()

  @newMain def multipleSameNames2(
    @alias("arg") @alias("arg") num: Int,
    inc: Int
  ): Unit = ()

  @newMain def multipleSameNames3(
    num: Int,
    @alias("num") inc: Int
  ): Unit = ()
end myProgram


object Test:
  def hasCauseIllegalArgumentException(e: Throwable): Boolean =
    e.getCause match {
      case null => false
      case _: IllegalArgumentException => true
      case e: Throwable => hasCauseIllegalArgumentException(e)
    }

  def callMain(className: String, args: Array[String]) =
    val clazz = Class.forName(className)
    val method = clazz.getMethod("main", classOf[Array[String]])

    try { method.invoke(null, args) }
    catch {
      case e: Exception if hasCauseIllegalArgumentException(e) => println("OK")
    }

  def main(args: Array[String]): Unit =
    callMain("multipleSameShortNames1", Array("--num", "2", "--inc", "3"))
    callMain("multipleSameShortNames1", Array("-n", "2", "--inc", "3"))
    callMain("multipleSameShortNames2", Array("--num", "2", "--inc", "3"))
    callMain("multipleSameShortNames2", Array("-n", "2", "--inc", "3"))

    callMain("multipleSameNames1", Array("--num", "2", "--inc", "3"))
    callMain("multipleSameNames1", Array("--arg", "2", "--inc", "3"))
    callMain("multipleSameNames2", Array("--num", "2", "--inc", "3"))
    callMain("multipleSameNames2", Array("--arg", "2", "--inc", "3"))
    callMain("multipleSameNames3", Array("--num", "2", "--inc", "3"))
end Test
