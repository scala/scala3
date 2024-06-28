opaque type Foo[A] <: A = A

class Test:
  def t1(x: Option[Foo[List[Int]]]): Unit = x match
    case Some(foo) =>
    case None      =>
