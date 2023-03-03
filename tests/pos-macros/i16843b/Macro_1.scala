import scala.quoted.*

inline def foo: Int = ${ fooImpl }

def fooImpl(using Quotes): Expr[Int] =
  '{
    val b = ${
      val a = '{
        (1: Int) match
          case x @ (y: Int) => 0
      }
      a
    }

    (1: Int) match
      case x @ (y: Int) => 0
  }

