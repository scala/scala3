import scala.quoted._

inline def test[T[_]]: Unit = ${ testExpr[T] }

def testExpr[T[_]](using s: Scope)(using s.Type[T]): s.Expr[Unit] = {
  import s.tasty._
  def variance(f: Flags) =
     if f.is(Flags.Covariant) then "+"
     else if f.is(Flags.Contravariant) then "-"
     else " "
  val t = '[T].tpe.typeSymbol.typeMembers.map(x => (x.name, variance(x.flags)))
  '{ println(${Expr(t.toString)}) }
}
