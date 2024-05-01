object Test:
  import scala.annotation.experimental

  @experimental def main(args: Array[String]): Unit =
    val s1: String | Null = "hello"
    val s2: String | Null = null

    val opts1: Option[String] = Option.fromNullable(s1)
    val opts2: Option[String] = Option.fromNullable(s2)

    opts1 match
      case Some(s) => println(s)
      case None => println("None")

    opts2 match
      case Some(s) => println(s)
      case None => println("None")
