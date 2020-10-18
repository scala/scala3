package dotty.tools
package dotc
package core

import Contexts._, Symbols._, Types._, Flags._, Scopes._, Decorators._, NameOps._
import Denotations._
import SymDenotations.LazyType, Names.Name, StdNames.nme

/** Operations that are shared between Namer and TreeUnpickler */
object NamerOps:

  /** The given type, unless `sym` is a constructor, in which case the
   *  type of the constructed instance is returned
   */
  def effectiveResultType(sym: Symbol, typeParams: List[Symbol], givenTp: Type)(using Context): Type =
    if (sym.name == nme.CONSTRUCTOR) sym.owner.typeRef.appliedTo(typeParams.map(_.typeRef))
    else givenTp

  /** if isConstructor, make sure it has one non-implicit parameter list */
  def normalizeIfConstructor(termParamss: List[List[Symbol]], isConstructor: Boolean)(using Context): List[List[Symbol]] =
    if (isConstructor &&
      (termParamss.isEmpty || termParamss.head.nonEmpty && termParamss.head.head.isOneOf(GivenOrImplicit)))
      Nil :: termParamss
    else
      termParamss

  /** The method type corresponding to given parameters and result type */
  def methodType(typeParams: List[Symbol], valueParamss: List[List[Symbol]], resultType: Type, isJava: Boolean = false)(using Context): Type =
    val monotpe =
      valueParamss.foldRight(resultType) { (params, resultType) =>
        val (isContextual, isImplicit, isErased) =
          if params.isEmpty then (false, false, false)
          else (params.head.is(Given), params.head.is(Implicit), params.head.is(Erased))
        val make = MethodType.companion(isJava = isJava, isContextual = isContextual, isImplicit = isImplicit, isErased = isErased)
        if isJava then
          for param <- params do
            if param.info.isDirectRef(defn.ObjectClass) then param.info = defn.AnyType
        make.fromSymbols(params, resultType)
      }
    if typeParams.nonEmpty then PolyType.fromParams(typeParams.asInstanceOf[List[TypeSymbol]], monotpe)
    else if valueParamss.isEmpty then ExprType(monotpe)
    else monotpe

  /** Add moduleClass or sourceModule functionality to completer
   *  for a module or module class
   */
  def adjustModuleCompleter(completer: LazyType, name: Name)(using Context): LazyType =
    val scope = ctx.effectiveScope
    if name.isTermName then
      completer.withModuleClass(findModuleBuddy(name.moduleClassName, scope))
    else
      completer.withSourceModule(findModuleBuddy(name.sourceModuleName, scope))

  /** Find moduleClass/sourceModule in effective scope */
  private def findModuleBuddy(name: Name, scope: Scope)(using Context) = {
    val it = scope.lookupAll(name).filter(_.is(Module))
    if (it.hasNext) it.next()
    else NoSymbol.assertingErrorsReported(s"no companion $name in $scope")
  }

end NamerOps