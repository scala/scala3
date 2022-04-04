import scala.util.CommandLineParser.FromString

given [T : FromString]: FromString[Option[T]] with
  override def fromString(s: String) = Some(summon[FromString[T]].fromString(s))
  override def fromStringOption(s: String) =
    try {
      Some(fromString(s))
    }
    catch {
      case _: IllegalArgumentException => Some(None)
    }

given [T : FromString]: FromString[Either[T, String]] with
  override def fromString(s: String) = Left(summon[FromString[T]].fromString(s))
  override def fromStringOption(s: String) =
    try {
      Some(fromString(s))
    }
    catch {
      case _: IllegalArgumentException => Some(Right(s"Unable to parse argument $s"))
    }

object myProgram:

  @main def getOption(o: Option[Int] = Some(42)) = println(o)

  @main def getEither(e: Either[Int, String] = Right("No argument given")) = println(e)

end myProgram


object Test:
  def call(className: String, args: Array[String]): Unit =
    val clazz = Class.forName(className)
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    call("getOption", Array("7"))
    call("getOption", Array())
    call("getOption", Array("abc"))
    println
    call("getEither", Array("7"))
    call("getEither", Array())
    call("getEither", Array("abc"))
end Test
