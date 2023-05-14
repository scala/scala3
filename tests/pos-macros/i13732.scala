import scala.quoted.*

def generateImpl(using q: Quotes): Expr[Unit] =
  '{ def runEffect[T]: T = ${ runEffectImpl[T] } }

inline def runEffectImpl[T: Type]: Expr[T] = ???
