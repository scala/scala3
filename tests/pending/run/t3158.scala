object Test {
  def main(args: Array[String]): Unit = {
    println(args.map(_ => foo _).deep)
  }

  def foo(xs: String*): Unit = {
    println(xs)
  }
}
