trait ::[H, T]

trait Foo[A, R]

trait FooLowPrio {
  implicit def caseOther[A]: Foo[A, A :: Any] = null
}
object Foo extends FooLowPrio {
  implicit def caseCons[H, HR, T, TR]
    (implicit // It's a bit artificial: the by name is not required in this example...
      t: => Foo[T, TR],
      h: => Foo[H, HR]
    ): Foo[H :: T, TR] = null

  implicit def caseAny: Foo[Any, Any] = null
}

object Test {
  val implicitFoo = implicitly[Foo[Long :: Any, Any]]
}
