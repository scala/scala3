import scala.quoted.*

inline def valToFun[T](inline expr: T): T =
  ${ impl('expr) }

def impl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[T] =
  expr match
    case '{ type α; { val ident = ($a: `α`); $rest(ident): `α` & T } } =>
      '{ { (y: α) => $rest(y) }.apply(???) }
