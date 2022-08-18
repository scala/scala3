// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    A.foo(1)
    A.foo(Array(1)*)
    A.foo(Seq(1)*)
    A.gen(1)
    A.gen(Array(1)*)
    A.gen(Seq(1)*)
    A.gen2("")
    A.gen2(Array("")*)
    A.gen2(Seq("")*)
  }
}
