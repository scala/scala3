import scala.language.unsafeNulls
class C {
  def g: String | Null = ???

  def f =
    try g catch case _ => ""

  def f2 = if ??? then g else ""

  def f3 = (??? : Boolean) match
    case true => g
    case _ => ""
}