class Test {

  object Foo { def unapply(x: Int) = if (x == 2) Some(x.toString) else None }

  def test: Unit = {
    val a: PartialFunction[Int, String] = { case Foo(x) => x }
    val b: PartialFunction[Int, String] = { case x => x.toString }

    val e: PartialFunction[String, String] = { case x @ "abc" => x }
    val f: PartialFunction[String, String] = x => x match { case "abc" => x }
    val g: PartialFunction[String, String] = x => x match { case "abc" if x.isEmpty => x }

    type P = PartialFunction[String,String]
    val h: P = { case x => x.toString }

    val i: PartialFunction[Int, Int] = { x => x match { case x => x } }
  }
}
