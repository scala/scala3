// https://github.com/scala/scala3/issues/7414

object DepTest {
  trait Trait {
    case class Dependent()
  }
  object obj extends Trait
  case class Dep[T <: Trait](t: T) {
    def fun(q: t.Dependent): Unit = ???
  }
  Dep(obj).fun(obj.Dependent())
}
