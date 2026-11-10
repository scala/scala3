trait Binder[T]

object Binder:
  def foo(s: String): Binder[Int] = new Binder[Int] {}
  val foo: Binder[Int] = foo("x")

// Named `binder =` skips the defaulted `name` parameter.
case class Box[A](tpe: String, name: String = "", binder: Binder[A] = null)

object Mapping:
  inline def localDate: Box[Int] =
    Box("LocalDate", binder = Binder.foo)
