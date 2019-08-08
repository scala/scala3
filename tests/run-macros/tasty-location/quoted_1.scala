import scala.quoted._
import given scala.quoted.autolift._

case class Location(owners: List[String])

object Location {

  implicit inline def location: Location = ${impl}

  def impl given (qctx: QuoteContext): Expr[Location] = {
    import qctx.tasty._

    def listOwnerNames(sym: Symbol, acc: List[String]): List[String] =
      if (sym == defn.RootClass || sym == defn.EmptyPackageClass) acc
      else listOwnerNames(sym.owner, sym.name :: acc)

    val list = listOwnerNames(rootContext.owner, Nil)
    '{new Location(${list})}
  }

}
