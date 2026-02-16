object Repro {
  inline def apply(): Unit = ${ applyImpl }

  import scala.quoted.*
  def applyImpl(using q: Quotes): Expr[Unit] = {
   import q.reflect.*
   report.info(TypeRepr.of[Some[String]].typeSymbol.pos.toString)
   '{ () }
  }
}
