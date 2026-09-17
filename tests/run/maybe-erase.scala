//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.Ok

def f(x: String?): String? = x
def f1(x: String?): String = x match
  case Ok(y) => y
  case null => "none"

def f2[T <: String](x: T?): String = x match
  case Ok(y) => y
  case null => "none"

def f3[T <: String?](x: T): String = x match
  case Ok(y) => y
  case null => "none"

@main def Test =
  val s: String? = "a"
  val b = s.isEmpty
  assert(!b)

  assert(f(s) == s)
  assert(f1(s) == s)
  assert(f1(null) == "none")
  assert(f2(s) == s)
  assert(f2(null) == "none")
  assert(f3(s) == s)
  assert(f3(null) == "none")

