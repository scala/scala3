class Test {
  val v1 = (if true then Some(1) else None).map(v => v+1)
  val v2 = (try Some(1) finally {}).map(v => v+1)
  val v3 = (1 match { case _ => Some(1) }).map(v => v+1)
  val v4 = (while (true) ()).toString
  def v5: Option[String] = Some((return Some("a")).toString)
  def foo(x: Boolean) = !x
  def bar() = (if true then 1 else 2) match { case x => x }
  def baz() = (if true then identity else identity)(0)
}
