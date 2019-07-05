package blah

import scala.quoted._

object A {
  inline def f: Unit = ${impl}
  private def impl given (qctx: QuoteContext): Expr[Unit] = {
    '{println("A.f")}
  }
  object B {
    inline def f: Unit = ${impl}
    private def impl given (qctx: QuoteContext): Expr[Unit] = {
      '{println("A.B.f")}
    }
    object C {
      inline def f: Unit = ${impl}
      private def impl given (qctx: QuoteContext): Expr[Unit] = {
        '{println("A.B.C.f")}
      }
    }
  }
}
