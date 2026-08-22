//> using options -Yexplicit-nulls
import language.experimental.magic

var x: String? = "hi"
var y: String | Null = "ho"

def Test =
  x = y
  y = x

  val z1 = if ??? then x else y
  val _: String? = z1
  val z2 = if ??? then y else x
  val _: String? = z2

  def foo[T](x: T, y: T): T = x

  val z3 = foo(x, y)
  val _: String? = z3
