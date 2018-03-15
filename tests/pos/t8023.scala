class C[K]
class D[K]

object Test3 {
  def foo = (null: Any) match {
    case a: C[type K] => new C[K]() // this one worked before as the info of `A` was complete
    // ()
  }
}
