import scala.quoted.{ Expr, Quotes }

package macros:
  trait Macros:
    import Macros.*
    protected inline def test(d: Outer.Derived): Unit =
      ${ testCode('d) }

  object Macros:
    def testCode(d: Expr[Outer.Derived])(using Quotes): Expr[Unit] = '{ assert(true, $d.any) }
