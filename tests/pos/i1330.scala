class Test {
  val ok = "ok"
  val s: ok.type = ok match { case x => x }
  val ts: ok.type = (ok : @unchecked) match { case x => x }

  def foo: PartialFunction[Int, Unit] = {
    case x => ()
  }
}
