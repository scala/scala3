import scala.quoted.*

object Test {
  inline def testMacro: Unit = ${ testMacroImpl }

  private def testMacroImpl(using q: Quotes): Expr[Unit] = {
    def makeInner(using q: Quotes): Expr[Any] = '{
      val t: Int = ${
        val tSymbol = quotes.reflect.Symbol.spliceOwner
        assert(tSymbol.owner == q.reflect.Symbol.spliceOwner) // used to fail on the second expansion
        '{ 123 }
      }
    }

    def makeOuter(using Quotes): Expr[Any] = '{
      new Runnable {
        override def run(): Unit = ${ makeInner }
      }
    }

    '{
      ${ makeOuter }
      ${ makeOuter }
      ()
    }
  }
}
