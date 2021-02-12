import scala.quoted.*

inline def test[T[_]]: Unit = ${ testExpr[T] }

def testExpr[T[_]: Type](using Quotes): Expr[Unit] = {
  import quotes.reflect.*
  def variance(f: Flags) =
     if f.is(Flags.Covariant) then "+"
     else if f.is(Flags.Contravariant) then "-"
     else " "
  val t = TypeRepr.of[T].typeSymbol.memberTypes.map(x => (x.name, variance(x.flags)))
  '{ println(${Expr(t.toString)}) }
}
