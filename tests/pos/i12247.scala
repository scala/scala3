sealed abstract class CtorType
object CtorType {
  final class Props extends CtorType
  sealed trait Summoner { type CT <: CtorType }
  implicit def summonP: Summoner {type CT = Props} = ???
}

final case class Builder() {
  def build(using ctorType: CtorType.Summoner): Component[ctorType.CT] = ???
}

final class Component[CT <: CtorType]

object Test {

  def assertTypeOf[A](a: => A) = new TestDsl[A]
  class TestDsl[A] {
    def is[B](implicit ev: A =:= B): Unit = ()
  }

  type Expect = Component[CtorType.Props]

  assertTypeOf( Builder().build ).is[Expect] // error

  val x = Builder().build
  assertTypeOf(x).is[Expect] // ok
}
