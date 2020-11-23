import scala.quoted._

inline def test[T[_]]: Unit = ${ testExpr[T] }

def testExpr[T[_]: Type](using Quotes): Expr[Unit] = {
  import qctx.reflect._
  def variance(f: Flags) =
     if f.is(Flags.Covariant) then "+"
     else if f.is(Flags.Contravariant) then "-"
     else " "
  val t = TypeRepr.of[T].typeSymbol.typeMembers.map(x => (x.name, variance(x.flags)))
  '{ println(${Expr(t.toString)}) }
}
