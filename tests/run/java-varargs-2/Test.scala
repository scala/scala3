object Test {
  def main(args: Array[String]): Unit = {
    A.foo(1)
    A.foo(Array(1): _*)
    A.foo(Seq(1): _*)
    A.gen(1)
    A.gen(Array(1): _*)
    A.gen(Seq(1): _*)
    A.gen2("")
    A.gen2(Array(""): _*)
    A.gen2(Seq(""): _*)
  }
}
