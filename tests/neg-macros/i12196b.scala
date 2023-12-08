import scala.quoted.*

def qqq(s: String)(using Quotes): Expr[Unit] = '{()}

abstract class Foo:
  inline val InlineVal: String

val foo: Foo = ???

inline def withInlineVal = ${ qqq(foo.InlineVal) } // error
