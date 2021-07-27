import scala.quoted.*

inline def testSubst: Unit = ${ testSubstImpl }

def testSubstImpl(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  val intTpe = TypeRepr.of[Int]
  val strOptTpe = TypeRepr.of[Option[String]]

  val tpeArgs: List[TypeRepr] = strOptTpe match {
    case AppliedType(_, args) => args
    case _                    => List.empty[TypeRepr]
  }

  val intOptTpe = strOptTpe.substituteTypes(
    tpeArgs.map(_.typeSymbol), List(intTpe))

  val repr = s"${strOptTpe.show} to ${intOptTpe.show}"

  '{
    println(${Expr(repr)})
  }
}
