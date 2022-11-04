// scalajs: --skip

import scala.annotation.newMain
import scala.annotation.newMain.alias
import scala.util.Try

object myProgram:

  /** A method that should let --help and -h display help. */
  @newMain def helpOverride1(notHelp: Int) = ???

  /** A method that should let -h display help, but not --help. */
  @newMain def helpOverride2(help: Int) = ???

  /** A method that should let --help display help, but not -h. */
  @newMain def helpOverride3(h: Int) = ???

  /** A method that should not let --help and -h display help. */
  @newMain def helpOverride4(help: Int, h: Int) = ???


  /** A method that should let -h display help, but not --help. */
  @newMain def helpOverride5(@alias("help") notHelp: Int) = ???

  /** A method that should let --help display help, but not -h. */
  @newMain def helpOverride6(@alias("h") notHelp: Int) = ???

  /** A method that should not let --help and -h display help. */
  @newMain def helpOverride7(@alias("help") notHelp: Int, @alias("h") notH: Int) = ???

  /** A method that should not let --help and -h display help. */
  @newMain def helpOverride8(@alias("help") @alias("h") notHelp: Int) = ???

  /** A method that should not let --help and -h display help. */
  // Probably the correct way to override help flags.
  @newMain def helpOverride9(@alias("h") help: Boolean) = println(s"helpOverride9: $help")

end myProgram

object Test:
  val allClazzes: Seq[Class[?]] =
    LazyList.from(1).map(i => Try(Class.forName("helpOverride" + i.toString))).takeWhile(_.isSuccess).map(_.get)

  def callAllMains(args: Array[String]): Unit =
    for (clazz <- allClazzes) {
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, args)
    }

  def main(args: Array[String]): Unit =
    println("##### --help")
    callAllMains(Array("--help"))
    println("##### -h")
    callAllMains(Array("-h"))
end Test
