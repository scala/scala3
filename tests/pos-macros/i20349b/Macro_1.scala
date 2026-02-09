import scala.quoted.*

object Macros {


  def valuesImpl[A: Type](using quotes: Quotes): Expr[List[A]] = {
    import quotes.*, quotes.reflect.*

    extension (sym: Symbol)
     def isPublic: Boolean = !sym.isNoSymbol &&
          !(sym.flags.is(Flags.Private) || sym.flags.is(Flags.PrivateLocal) || sym.flags.is(Flags.Protected) ||
            sym.privateWithin.isDefined || sym.protectedWithin.isDefined)

    def isSealed[A: Type]: Boolean =
      TypeRepr.of[A].typeSymbol.flags.is(Flags.Sealed)

    def extractSealedSubtypes[A: Type]: List[Type[?]] = {
      def extractRecursively(sym: Symbol): List[Symbol] =
        if sym.flags.is(Flags.Sealed) then sym.children.flatMap(extractRecursively)
        else if sym.flags.is(Flags.Enum) then List(sym.typeRef.typeSymbol)
        else if sym.flags.is(Flags.Module) then List(sym.typeRef.typeSymbol.moduleClass)
        else List(sym)

      extractRecursively(TypeRepr.of[A].typeSymbol).distinct.map(typeSymbol =>
        typeSymbol.typeRef.asType
      )
    }

    if isSealed[A] then {
      val refs = extractSealedSubtypes[A].flatMap { tpe =>
        val sym = TypeRepr.of(using tpe).typeSymbol
        val isCaseVal = sym.isPublic && sym.flags
          .is(Flags.Case | Flags.Enum) && (sym.flags.is(Flags.JavaStatic) || sym.flags.is(Flags.StableRealizable))

        if (isCaseVal) then List(Ref(sym).asExprOf[A])
        else Nil
      }
      Expr.ofList(refs)
    } else '{ Nil }
  }

  inline def values[A]: List[A] = ${ valuesImpl[A] }
}
