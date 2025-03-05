import scala.quoted._

inline def test(): Any = ${ testImpl }

def testImpl(using Quotes): Expr[Any] =
  import quotes.reflect._
  TypeBlock(Nil, TypeTree.of[Int]) match
    case Block(_, res) =>
      res.asExpr // unexpected case - would crash here, as res of TypeBlock is not a term
    case _ =>
      '{()} // expected case
