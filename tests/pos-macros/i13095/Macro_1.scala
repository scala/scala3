
import scala.quoted.*

inline def testMacro = ${ test }

def test(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  assert(!Symbol.noSymbol.isDefinedInCurrentRun)
  '{}
