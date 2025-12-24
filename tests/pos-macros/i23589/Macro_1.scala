import scala.quoted.*
import scala.language.experimental.quotedPatternsWithPolymorphicFunctions

inline def testMacro(inline body: Any) = ${test('body)}
def test(outsideBody: Expr[Any])(using Quotes): Expr[Unit] =
  val insideBody = '{[B] => (a : B, b : B) => (a, b)}
  outsideBody match
    case '{ [A] => (x : A, y : A) => $b[A](x, y): (A, A) } => ()
  insideBody match
    case '{ [A] => (x : A, y : A) => $b[A](x, y): (A, A) } => ()
  '{()}
