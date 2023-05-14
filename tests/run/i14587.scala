def test(foo: String): Unit = {
  // Does not crash if the type is written explicitly as: Option[(Option[Int], String)]
  val bar = {
    if (foo.isEmpty) Some((Some(1), ""))
    else Some((None, ""))
  }

  bar.foreach {
    case (Some(_), "") =>
    case _ =>
  }
}

@main def Test() =
  test("") // works
  test("a")
