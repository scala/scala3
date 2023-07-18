class Has[A]
trait Foo

class TestAspect[+LowerR, -UpperR]

class Spec[-R] {
  def foo[R1 <: R](aspect: TestAspect[R1, R1]): Unit = {}
}

class SuiteBuilder[R <: Has[_]] {
  def toSpec(
    spec: Spec[R & Has[Foo]],
    aspect: TestAspect[
      R & Has[Foo],
      R & Has[Foo]
    ]
  ) =
    spec.foo(aspect)
}