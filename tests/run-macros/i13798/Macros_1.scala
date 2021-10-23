import scala.quoted.*

inline def testCompanion: Unit = ${ testCompanionImpl }

def testCompanionImpl(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  val tpe = TypeRepr.of[Tuple]
  val sym: TypeRepr = tpe.companion
  val repr = sym.typeSymbol.fullName

  '{ println(${Expr(repr)}) }
}
