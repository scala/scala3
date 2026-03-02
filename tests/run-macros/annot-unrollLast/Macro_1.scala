//> using options -experimental

package example

import scala.annotation.{experimental, MacroAnnotation, StaticAnnotation}
import scala.quoted._
import scala.collection.mutable.Map
import scala.compiletime.ops.double

// TODO make unrollLast the macro annotation and remove unrollHelper
class unrollLast extends StaticAnnotation

@experimental
class unrollHelper extends MacroAnnotation {
  def transform(using Quotes)(tree: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    import quotes.reflect._
    tree match
      case tree: DefDef => transformDefDef(tree)
      case _ => report.throwError("unrollHelper can only be applied to a method definition", tree.pos)

  private def transformDefDef(using Quotes)(ddef: quotes.reflect.DefDef): List[quotes.reflect.Definition] =
    import quotes.reflect._
    val unrollLastSym = Symbol.requiredClass("example.unrollLast")
    ddef.paramss match
      case Nil =>
        report.throwError("unrollHelper must have an @unrollLast parameter", ddef.pos)
      case _ :: _ :: _ =>
        report.throwError("unrollHelper does not yet support multiple parameter lists", ddef.pos)
      case TermParamClause(params) :: Nil =>
        if params.isEmpty then report.throwError("unrollHelper must have an @unrollLast parameter", ddef.pos)
        else if params.init.exists(_.symbol.hasAnnotation(unrollLastSym)) || !params.last.symbol.hasAnnotation(unrollLastSym) then
          report.throwError("@unrollLast must be on the last parameter", ddef.pos)
        List(ddef, makeTelescopedDefDefWithoutLastArgument(ddef.symbol))
      case _ =>
        report.throwError("unrollHelper does not yet support type parameters", ddef.pos)

  private def makeTelescopedDefDefWithoutLastArgument(using Quotes)(defSym: quotes.reflect.Symbol): quotes.reflect.DefDef =
    import quotes.reflect._
    def ddef1Rhs(argss: List[List[Tree]]): Some[Term] =
      val defaultArg = defaultGetter(defSym, argss.size + 2) // +1 for 1-based and +1 for the argument that was dropped
      val args1 = argss.head.asInstanceOf[List[Term]] :+ defaultArg
      Some(Ref(defSym).appliedToArgs(args1))
    val sym1 = makeTelescopedSymbolWithoutLastArgument(defSym)
    DefDef(sym1, ddef1Rhs)

  private def makeTelescopedSymbolWithoutLastArgument(using Quotes)(defSym: quotes.reflect.Symbol): quotes.reflect.Symbol =
    import quotes.reflect._
    val info1 = defSym.info match
      case info: MethodType => MethodType(info.paramNames.init)(_ => info.paramTypes.init, _ => info.resType)
    Symbol.newMethod(defSym.owner, defSym.name, info1, Flags.EmptyFlags, Symbol.noSymbol)

  private def defaultGetter(using Quotes)(sym: quotes.reflect.Symbol, idx: Int): quotes.reflect.Term =
    import quotes.reflect._
    val getterSym = sym.owner.methodMember(sym.name + "$default$" + idx).head
    Ref(getterSym)
}
