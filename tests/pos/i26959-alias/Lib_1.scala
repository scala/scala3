package repro

// https://github.com/scala/scala3/issues/26959
trait Outer {
  type U = Unit
  type ID[T] = T
  trait InnerSupport { this: Companion.type =>
    def foo: U = ()
    def bar: ID[Unit] = ()
  }
  val Companion: CompanionTrait
  trait CompanionTrait extends InnerSupport { this: Companion.type => }
}
