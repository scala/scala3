import scala.quoted.*

object MatchMac {

    inline def apply(inline any: Any): Unit = ${ printMacImpl('any) }

    def printMacImpl(any: Expr[Any])(using Quotes): Expr[Unit] = {
      import quotes.reflect.*
      val res = any match {
        case '{ ($f: Person).name } => "matched!"
        case _ => "not matched"
      }
      '{ println(${Expr(res)}) }
    }

}
