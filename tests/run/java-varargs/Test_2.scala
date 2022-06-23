// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    A_1.foo(1)
    A_1.foo(Array(1)*)
    A_1.foo(Seq(1)*)
    A_1.gen(1)
    A_1.gen(Array(1)*)
    A_1.gen(Seq(1)*)
    A_1.gen2("")
    A_1.gen2(Array("")*)
    A_1.gen2(Seq("")*)
  }
}
