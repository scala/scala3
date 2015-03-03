class A {
  implicit def compareComparables[T](x: T)(implicit ord: Ordering[T]): ord.Ops = new ord.Ops(x)

  class Bippy
  implicit val bippyOrdering: Ordering[A.this.Bippy] = new Ordering[Bippy] { def compare(x: Bippy, y: Bippy) = util.Random.nextInt }

  (new Bippy) < (new Bippy)
}
