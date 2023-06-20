import scala.quoted._

trait ReflectModule {
  implicit def q: Quotes
  import quotes.reflect._ // error

  def foo(x: TypeRepr): Unit = ???
}