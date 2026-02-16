trait Outer[O1]:
  sealed trait Foo[A1]
  final class Bar[A2] extends Foo[A2]
  final class Baz[A4] extends Foo[Bar[A4]]
  final class Qux     extends Foo[[a5] => Foo[a5] => Foo[a5]]

trait Test[O2]:
  val outer: Outer[O2]
  import outer.*

  def test[X](fa: Foo[X]): Unit =
    fa match // was: inexhaustive: fail on _: (Outer[<?>] & (Test#outer : Outer[Test#O2]))#Qux
      case _: Bar[X] => ???
      case _: Baz[x] => ??? // was: unrearchable warning
      case _: Qux    => ??? // was: unrearchable warning
