class C[K]
class D[K]

object Test3 {
  def foo = (null: Any) match {
    case a: C[k] => new C[k]() // this one worked before as the info of `A` was complete
    // ()
  }
}
