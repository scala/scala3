object StringContext {
  val i = 42
  val s = s"Answer: $i"
}

object Test {
  def main(args: Array[String]): Unit = {
    println(StringContext.s)
  }
}