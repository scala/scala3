class Test {
  def test: Unit = {
    val a: PartialFunction[Int, Int]{ def foo: Int } = { case x => x } // error

    type PF = PartialFunction[Int, Int] { def foo: Int }
    val b: PF = { case x => x } // error

    type PF1 = PartialFunction[Int, Int] {def foo: Int }
    val c: PF1 {} = { case x => x } // error

    type PF2 = PartialFunction[Int, Int]
    val d: PF2 {} = { case x => x }

    type PF3 = PartialFunction[Int, Int] {}
    val e: PF3 = { case x => x }

    val f: PartialFunction[Int, Int] {} { def foo: Int } = { case x => x } // error
  }
}
