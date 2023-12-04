enum Foo:
  case A

package bar {
  enum Bar:
    case B
}

@main def Test =
  import scala.util.Try
  println(Try(Foo.fromOrdinal(3)))
  println(Try(Foo.valueOf("Bar")))
  println(Try(bar.Bar.fromOrdinal(4)))
  println(Try(bar.Bar.valueOf("Baz")))
