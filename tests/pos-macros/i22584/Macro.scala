//> using options -Yretain-trees

import scala.quoted.*

object Macros {

  inline def myMacro[A]: Unit = ${ myMacroImpl[A] }

  private def myMacroImpl[A](using quotes: Quotes, tpe: Type[A]): Expr[Unit] = {
    import quotes.reflect.*

    val typeRepr = TypeRepr.of[A]
    val typeSymbol = typeRepr.typeSymbol

    val caseFieldSymbols: List[Symbol] = typeSymbol.fieldMembers
    val caseFieldValOrDefDefs: List[ValDef | DefDef] =
      caseFieldSymbols.sortBy(_.toString).map {
        _.tree match {
          case valDef: ValDef => valDef
          case defDef: DefDef => defDef
          case _              => report.errorAndAbort("???")
        }
      }

    val expected = "List(DefDef(boolean,List(List()),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Boolean)],Select(This(Ident(MyClass1)),boolean)), DefDef(finalVal,List(List()),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class String)],Select(This(Ident(MyClass1)),finalVal)), DefDef(int,List(List()),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],Select(This(Ident(MyClass1)),int)), DefDef(string,List(List()),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class String)],Select(This(Ident(MyClass1)),string)))"
    assert(caseFieldValOrDefDefs.toString == expected)

    '{ () }
  }
}
