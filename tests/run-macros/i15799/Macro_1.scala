import scala.quoted.*

object exampleMacro {

  inline def typeDefRhs[T <: AnyKind]: (String, String) = ${ typeDefRhsImpl[T] }

  def typeDefRhsImpl[T <: AnyKind: Type](using Quotes): Expr[(String, String)] = {
    import quotes.reflect.*
    val underlyingTpe =
      TypeRepr.of[T] match
        case tpe: TypeRef => tpe.underlying
    Expr((Type.show[T], underlyingTpe.show))
  }
}
