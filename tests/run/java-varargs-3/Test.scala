object Test {
  def main(args: Array[String]): Unit = {
    val ai = new A[Int]
    ai.gen(1)
    ai.gen2(1)
    ai.gen(Array(1)*)
    ai.gen2(Array(1)*)
    ai.gen(Seq(1)*)
    ai.gen2(Seq(1)*)

    val b = new B[String]
    b.gen("")
    b.gen2("")
    b.gen(Array("")*)
    b.gen2(Array("")*)
    b.gen(Seq("")*)
    b.gen2(Seq("")*)
  }
}
