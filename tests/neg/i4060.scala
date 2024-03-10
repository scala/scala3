//> using options -language:experimental.erasedDefinitions

// See https://github.com/scala/scala3/issues/4060#issuecomment-445808377

object App {
  trait A { type L >: Any}
  def upcast(erased a: A)(x: Any): a.L = x
  erased val p: A { type L <: Nothing } = p
  def coerce(x: Any): Int = upcast(p)(x) // error

  def coerceInline(x: Any): Int = upcast(compiletime.erasedValue[A {type L <: Nothing}])(x) // error

  trait B { type L <: Nothing }
  def upcast_dep_parameter(erased a: B)(x: a.L) : Int = x
  erased val q : B { type L >: Any } = compiletime.erasedValue

  def coerceInlineWithB(x: Any): Int = upcast_dep_parameter(q)(x) // error

  def main(args: Array[String]): Unit = {
    println(coerce("Uh oh!"))
    println(coerceInlineWithB("Uh oh!"))
  }
}
