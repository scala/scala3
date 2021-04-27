object Test {
  class Foo(val name: String, val children: Int *)
  object Foo {
    def unapply(f: Foo) = Some((f.name, f.children))
  }

  def foo(f: Foo) = f match {
    case Foo(name, cs *) => name :: cs.reverse.toList.map(_.toString)
  }
  def main(args: Array[String]) = {
    println(foo(new Foo("hi", 1, 2, 3)).mkString(" "))
  }
}
