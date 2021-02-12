import scala.quoted.*

case class Location(owners: List[String])

object Location {

  implicit inline def location: Location = ${impl}

  def impl(using Quotes) : Expr[Location] = {
    import quotes.reflect.*

    def listOwnerNames(sym: Symbol, acc: List[String]): List[String] =
      if (sym == defn.RootClass || sym == defn.EmptyPackageClass) acc
      else listOwnerNames(sym.owner, sym.name :: acc)

    val list = listOwnerNames(Symbol.spliceOwner, Nil)
    '{new Location(${Expr(list)})}
  }

}
