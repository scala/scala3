// NOTE: this test case is very fragile, removing seemingly unrelated code like
// the "Dummy" trait somehow changes how the types of cget and dget are inferred
// and avoids the bug.

object Test {
  trait Dummy {
    def foo: Unit = {
      var i = 0
      i += 1
    }
  }

  trait FromIterator[+C[X] <: Iterable[X]] {
    def get(): C[Int]
  }

  trait Iterable[+IA] extends FromIterator[Iterable]
  trait SubIterable[+IA] extends Iterable[IA] with FromIterator[SubIterable]

  class IterableC extends Iterable[Int] { def get() = this }
  class SubIterableC extends SubIterable[Int] { def get() = this }


  implicit class IterableTransforms[A, C[X] <: Iterable[X], D[X] <: SubIterable[X]]
    (val dummy: Unit) {
    def foo(c: Iterable[A] with FromIterator[C], d: Iterable[A] with FromIterator[D]): Unit = {
      var cget = c.get()
      var dget = d.get()
      dget = cget // error
      cget = dget // error
    }
  }

  def main(args: Array[String]): Unit = {
    new IterableTransforms(()).foo(new IterableC, new SubIterableC)
    // java.lang.ClassCastException: Test$IterableC cannot be cast to Test$SubIterable
  }
}


