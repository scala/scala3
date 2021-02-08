import scala.quoted.*

def test[T: Type](x: Expr[Any])(using Quotes): Unit =

  x match
    case '{ $x: T } => // match using outer `T`
      '{ val y: T = $x; }

  x match
    case '{ $x: t } => // match any `t` and bind it as a type variable
      '{ val y: t = $x; } // use `t` provided by previous match

  x match
    case '{ $x: List[t] } => // match any `t` and bind it as a type variable
      '{ val y: List[t] = $x; } // use `t` provided by previous match

  x match
    case '{ $x: t } => // match any `t` and bind it as a type variable
      x match
        case '{ $x: `t` } => // match using outer `t` provided by previous match
          '{ val y: t = $x; }

  x match
    case '{ type t; $x: `t` } => // match any `t` and bind it as a type variable
      '{ val y: t = $x; } // use `t` provided by previous match

  Type.of[T] match
    case '[List[u]] => Type.of[u]
    case '[u] => Type.of[u]
