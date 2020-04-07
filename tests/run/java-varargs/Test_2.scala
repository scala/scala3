object Test {
  def main(args: Array[String]): Unit = {
    A_1.foo(Array(1): _*)
    A_1.foo(Seq(1): _*)
  }
}
