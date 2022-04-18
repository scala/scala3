import scala.language.unsafeNulls
class C {
  def g: String | Null = ???

  def f =
    if ??? then "" else g
}