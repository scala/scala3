class Test {
  def test: Unit = {
    val a: PartialFunction[Int, Int] = { case x => x }
    val b: PartialFunction[Int, Int] = x => x match { case 1 => 1; case _ => 2 }
    val c: PartialFunction[Int, Int] = x => { x match { case y => y } }
    val d: PartialFunction[Int, Int] = x => { { x match { case y => y } } }

    val e: PartialFunction[Int, Int] = x => { println("foo"); x match { case y => y } } // error
    val f: PartialFunction[Int, Int] = x => x // error
    val g: PartialFunction[Int, String] = { x => x.toString } // error
  }
}
