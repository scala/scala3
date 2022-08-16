object Iso:
  import scala.deriving.*, scala.quoted.*
  transparent inline def fields[S <: Product](using m: Mirror.ProductOf[S]): Int = ${ impl[S]('m) }
  private def impl[S <: Product](m: Expr[Mirror.ProductOf[S]])(using Quotes, Type[S]): Expr[Int] =
    import quotes.reflect.*
    m match
      case '{ type a <: Tuple; $m: Mirror.ProductOf[S] { type MirroredElemTypes = `a` } } => '{ 1 }
