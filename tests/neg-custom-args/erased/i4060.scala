// See https://github.com/lampepfl/dotty/issues/4060#issuecomment-445808377
import scala.language.experimental.erasedDefinitions

object App {
  trait A { type L >: Any}
  //def upcast(a: A, x: Any): a.L = x
  def upcast(erased a: A)(x: Any): a.L = x
  //lazy val p: A { type L <: Nothing } = p
  erased val p: A { type L <: Nothing } = p // error
  def coerce(x: Any): Int = upcast(p)(x)

  def coerceInline(x: Any): Int = upcast(compiletime.erasedValue[A {type L <: Nothing}])(x) // error

  def main(args: Array[String]): Unit = {
    println(coerce("Uh oh!"))
  }
}
