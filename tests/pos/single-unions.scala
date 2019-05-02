object A {
  val x1: 3 | 4 = 3
  val x2: 3 | 4 = 4
  val x3: 3 | 4 = if (???) 3 else 4
}

// The following example is from issue #1551
object Test1 {
  sealed trait Fence[+T, +S]
  case object End extends Fence[Nothing, Nothing]
  case class Post[+T, +S](value: T, next: Panel[T, S] | End.type) extends Fence[T, S]
  case class Panel[+T, +S](value: S, next: Post[T, S]) extends Fence[T, S]

  val fence = Post(1, Panel("two", Post(3, End)))
}
