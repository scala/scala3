
case class CC(a: String, b: String) extends Iterable[String] {
  override def iterator: Iterator[String] = Iterator(a, b)
}

trait T {
  extension (cc: CC) def className: String = "foo"
}

object O extends T {
  def foo = {
    val cc = CC("a", "b")
    println(cc.className)
  }
}

@main def main() = O.foo
