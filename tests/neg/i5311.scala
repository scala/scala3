object m {
  trait Foo {
    type T[A]
    def bar : (T[Int] => T[Int]) => T[Int => Int]
  }

  def baz (implicit S: Foo, f : S.T[Int] => S.T[Int]) : S.T[Int => Int] =
    S.bar.apply(f)

  def example(implicit s:Foo) : s.T[Int => Int] = {
    baz((x : s.T[Int]) => x) // error
  }
}