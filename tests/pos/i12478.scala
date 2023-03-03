sealed trait Foo[T]

object Foo:
  case class Bar[F[_]](fu: List[F[Unit]]) extends Foo[F[Unit]]

class Test:
  def id1[T1](foo1: Foo[T1]): Foo[T1] = foo1 match
    case Foo.Bar(fu) =>
      Foo.Bar(fu)

  def id2[T2](foo2: Foo[T2]): Foo[T2] = foo2 match
    case bar2 @ (_: Foo.Bar[f]) =>
      val fu2 = bar2.fu
      Foo.Bar(fu2)

  def id3[T3](foo3: Foo[T3]): Foo[T3] = foo3 match
    case bar3 @ Foo.Bar(_) =>
      val fu3 = bar3.fu
      Foo.Bar(fu3)
