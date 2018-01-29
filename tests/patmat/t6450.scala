sealed abstract class FoundNode[T]
case class A[T](x: T) extends FoundNode[T]

object Foo {
  val v: (Some[_], FoundNode[_]) = (???, ???)
  v match {
    case (x: Some[t], _) =>
  }
}
