package tuplelib

import scala.quoted.*

// 1: Inline body, re-typed when `mapAs` is inlined
extension (tup: Tuple)
  inline def mapAs[T]()[F[_ <: T]](inline f: [t <: T] => t => F[t])
      : Tuple.Map[tup.type, [X] =>> F[X & T]] =
    tup.map[[X] =>> F[X & T]]([t] => (t: t) => f(t.asInstanceOf[t & T]))

// 2: Quote pickled in the macro bytecode, unpickled when `m()` expands
inline def m(): Int = ${ mImpl }
def mImpl(using Quotes): Expr[Int] =
  '{ val f: [T] => T => T = [T] => (t: T) => t; f[Int](1) }

// 3: Annotation tree, unpickled when `annots[L.type]` forces it
class ann(x: Int) extends scala.annotation.StaticAnnotation
object L:
  @ann({ val f: [T] => T => T = [T] => (t: T) => t; f[Int](1) })
  def annotated: Int = 0

inline def annots[T](name: String): String = ${ annotsImpl[T]('name) }
def annotsImpl[T: Type](name: Expr[String])(using Quotes): Expr[String] =
  import quotes.reflect.*
  val sym = TypeRepr.of[T].typeSymbol.declaredMethod(name.valueOrAbort).head
  Expr(sym.annotations.map(_.show).mkString("\n"))
