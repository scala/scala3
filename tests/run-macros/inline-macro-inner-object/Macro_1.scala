package blah

import scala.quoted.*

object A {
  inline def f: Unit = ${impl}
  private def impl(using Quotes): Expr[Unit] = {
    '{println("A.f")}
  }
  object B {
    inline def f: Unit = ${impl}
    private def impl(using Quotes): Expr[Unit] = {
      '{println("A.B.f")}
    }
    object C {
      inline def f: Unit = ${impl}
      private def impl(using Quotes): Expr[Unit] = {
        '{println("A.B.C.f")}
      }
    }
  }
}
