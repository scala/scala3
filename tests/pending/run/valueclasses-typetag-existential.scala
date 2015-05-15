class Foo[T](val x: T) extends AnyVal

object Test extends dotty.runtime.LegacyApp {
  println(scala.reflect.runtime.universe.typeOf[Foo[_]])
}
