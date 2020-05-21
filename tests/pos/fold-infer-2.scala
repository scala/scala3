class Foo[A, B <: A] {
  def get: A = ???
  def hi: Foo[A, B] = this
}
class A {
  def foo[T <: Foo[_, _]]: T = ???
  foo.get // OK

  def list[T <: List[_]]: T = ???
  list.head: Int
  list.::(1)

  def foldLeft1[B <: List[_]](op: (B, Int) => B): B = ???
  val l = foldLeft1((acc, i) => acc.::(i))

  def foldLeft2[B >: Nil.type](op: (B, Int) => B): B = ???

  val l2 = foldLeft2((acc, i) => acc.::(i))

  extension fooOps on (x: List[Int]) {
    def hi: Int = 0
  }

  def foo1[B >: Foo[Int, Int]](op: B => B) = ???
  foo1(b => b.hi)

  def foo2[B >: List[Int]](op: B => Int) = ???
  foo2(b => b.hi)

  def foo3(xs: Seq[Option[List[Int]]]) =
    xs.map(_.getOrElse(Nil)).reduceLeft(_ ++ _)
}
