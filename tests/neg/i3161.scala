object UnionTypes {
  trait List[A]
  class Empty() extends List[Nothing]
  class Cons[A](h: A, t: List[A]) extends List[A]

  def test: Unit = {
    val list: Cons | Empty = null // error: missing type parameter
    val list2: Empty & Cons = ??? // error: missing type parameter
  }
}
