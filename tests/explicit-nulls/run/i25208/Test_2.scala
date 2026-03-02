import NonNullFold.*

object Test:
  def main(args: Array[String]): Unit =
    val x: String | Null = "not null"
    val y = x.foldNN("it's null")(x => x)
    println(y)
