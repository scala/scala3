object Test {
  def main(args: Array[String]): Unit = {
    println(args.map(_ => foo _).toList)
  }

  def foo(xs: String*): Unit = {
    println(xs)
  }
}
