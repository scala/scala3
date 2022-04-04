import scala.util.Try

object myProgram:

  /** A method that should let --help and -h display help. */
  @main def helpOverride1(notHelp: Int) = ???

  /** A method that should let -h display help, but not --help. */
  @main def helpOverride2(help: Int) = ???

  /** A method that should let --help display help, but not -h. */
  @main def helpOverride3(h: Int) = ???

  /** A method that should not let --help and -h display help. */
  @main def helpOverride4(help: Int, h: Int) = ???


  /** A method that should let -h display help, but not --help. */
  @main def helpOverride5(@main.Alias("help") notHelp: Int) = ???

  /** A method that should let --help display help, but not -h. */
  @main def helpOverride6(@main.Alias("h") notHelp: Int) = ???

  /** A method that should not let --help and -h display help. */
  @main def helpOverride7(@main.Alias("help") notHelp: Int, @main.Alias("h") notH: Int) = ???

  /** A method that should not let --help and -h display help. */
  @main def helpOverride8(@main.Alias("help", "h") notHelp: Int) = ???

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