package repro

// https://github.com/scala/scala3/issues/26959
trait Outer {
  trait InnerSupport { this: Companion.type =>
    def foo: Unit = ()
  }
  val Companion: CompanionTrait
  trait CompanionTrait extends InnerSupport { this: Companion.type => }
}

class User(c: Outer)
