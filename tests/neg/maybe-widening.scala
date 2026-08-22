//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*

def f(s: String): String? = s

def g[T](x: T): T? = x  // error

def g2[T](x: T): T? = Ok(x)

val x: String | Null = g("abc") // ok

def h[T](x: T?): T | Null = x // error

def h2[T](x: T?): T | Null = x match
  case Ok(y) => y
  case null => null
