import scala.quoted._

inline def test[T[_]]: Unit = ${ testExpr[T] }

def testExpr[T[_]: Type](using QuoteContext): Expr[Unit] = {
  import qctx.tasty._
  def variance(f: Flags) =
     if f.is(Flags.Covariant) then "+"
     else if f.is(Flags.Contravariant) then "-"
     else " "
  val t = quoted.Type[T].unseal.tpe.typeSymbol.typeMembers.map(x => (x.name, variance(x.flags)))
  '{ println(${Expr(t.toString)}) }
}
