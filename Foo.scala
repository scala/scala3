class a {

  def foo(x: Any) =

    new Foo { type T = Int; type Tup = (Int, List[Int]); val contents: Tup = (3, 5 :: Nil) } match {
      case Bar(x, ls) =>
        val l2 = x :: ls
        l2
    }
    new Foo { val contents = ??? } match {
      case Bar(x, ls) =>
        val l2 = x :: ls
        l2
    }
}



trait Foo {
  type T
  type Tup <: Tuple
  val contents: Tup
}

object Foo {
  def unaplly[F <: Foo](arg: F): Option[arg.Tup] = Some(arg.contents)
}

object Bar {
  def unapply(arg: Foo { type Tup = (T, List[T]) }): Option[arg.Tup] =
    Foo.unaplly[Foo { type Tup = (T, List[T]) }](arg)
}