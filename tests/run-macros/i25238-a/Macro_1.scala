import scala.quoted.*

sealed trait TupleModifier[Tup] {
  def rename: TupleModifier[Tup] & Regional[Tup]
  def renameNotRegional: TupleModifier[Tup]
}

sealed trait Regional[Tup]

object Regional {
  extension [Tup](self: TupleModifier[Tup] & Regional[Tup]) {
    def regional[Selected]: TupleModifier[Tup] = ???
  }
}

object ModifierParser {
  inline def use[A](inline mod: TupleModifier[A] => TupleModifier[A]): String = ${ parse('mod) }

  def parse[A](mod: Expr[TupleModifier[A] => TupleModifier[A]])(using Quotes): Expr[String] = {
    import quotes.reflect.*

    mod match {
      case '{ (builder: TupleModifier[tup]) => builder.renameNotRegional } =>
        Expr("renameNotRegional")
      case '{ (builder: TupleModifier[tup]) => builder.rename } =>
        Expr("rename")
      case '{ (builder: TupleModifier[tup]) => builder.rename.regional[a] } =>
        Expr("rename.regional")
      case _ => report.errorAndAbort("No match")
    }
  }
}
