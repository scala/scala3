// scalac: -Ycheck-all-patmat
object Test {
  val Nil: scala.collection.immutable.Nil.type = scala.collection.immutable.Nil
  val X = 5

  object Inner {
    val Y = false
  }

  def foo1a[T](l: List[T]) = l match {
    case x::xs => false
  }

  def foo1b[T](l: List[T]) = l match {
    case Nil => true
    case x::xs => false
  }

  def foo1c[T](l: List[T]) = l match {
    case Test.Nil => true
    case x::xs => false
  }

  def foo2(b: Boolean) = b match {
    case Inner.Y => false
  }

  def foo3(x: Int) = x match {
    case X => 0
  }
}
