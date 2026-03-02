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

    val expected = "List(ValDef(boolean,TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Boolean)],EmptyTree), ValDef(finalVal,Ident(String),Literal(Constant(result))), ValDef(int,TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)],EmptyTree), ValDef(string,TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String)],EmptyTree))"
    assert(caseFieldValOrDefDefs.toString == expected)

    '{ () }
  }
}
