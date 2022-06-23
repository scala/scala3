// scalajs: --skip

import scala.annotation.newMain
import scala.annotation.newMain.alias

import java.lang.reflect.InvocationTargetException

object myProgram:

  @newMain def empty(
    @alias("") i: Int,
  ): Unit = ()

  @newMain def space(
    @alias(" ") i: Int,
  ): Unit = ()

  @newMain def nonLetter(
    @alias("1") i: Int,
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
    try {
      method.invoke(null, args)
      println(s"Calling $className should result in an IllegalArgumentException being thrown")
    }
    catch {
      case e: InvocationTargetException if hasCauseIllegalArgumentException(e) => println("OK")
    }

  def main(args: Array[String]): Unit =
    callMain("empty", Array("3"))
    callMain("space", Array("3"))
    callMain("nonLetter", Array("3"))
end Test
