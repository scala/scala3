//> using options -language:experimental.specializedTraits
import scala.quoted.*

inline def describe[T]: String = ${ describeImpl[T] }

def describeImpl[T: Type](using Quotes): Expr[String] =
  Expr(Type.show[T])

inline trait A[T: Specialized]:
  val name: String = describe[T]
