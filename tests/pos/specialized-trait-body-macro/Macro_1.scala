//> using options -language:experimental.specializedTraits
import scala.quoted.*

inline def foo(): Int =
  ${fooImpl}

def fooImpl(using Quotes): Expr[Int] =
  '{3}

inline trait A[T: Specialized]:
  val i: Int = foo()
