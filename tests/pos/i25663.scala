object Foo:
  def unapplySeq(f: Int): Option[String *: Seq[Int] *: EmptyTuple] = ???

def foo(f: Int) = f match
  case Foo(name, ns*) => ???
