package dotty.tools
package dotc
package typer

import core.*
import Types.*, Contexts.*, Symbols.*, Flags.*, Constants.*
import reporting.*
import Decorators.i

/** A module for linter checks done at typer */
object Linter:
  import ast.tpd.*

  /** If -Wnonunit-statement is set, warn about statements in blocks that are non-unit expressions.
   *  @return  true if a warning was issued, false otherwise
   */
  def warnOnInterestingResultInStatement(t: Tree)(using Context): Boolean =

    def isUninterestingSymbol(sym: Symbol): Boolean =
      sym == NoSymbol ||
      sym.isConstructor ||
      sym.is(Package) ||
      sym.isPackageObject ||
      sym == defn.BoxedUnitClass ||
      sym == defn.AnyClass ||
      sym == defn.AnyRefAlias ||
      sym == defn.AnyValClass

    def isUninterestingType(tpe: Type): Boolean =
      tpe == NoType ||
      tpe.typeSymbol == defn.UnitClass ||
      defn.isBottomClass(tpe.typeSymbol) ||
      tpe =:= defn.UnitType ||
      tpe.typeSymbol == defn.BoxedUnitClass ||
      tpe =:= defn.AnyValType ||
      tpe =:= defn.AnyType ||
      tpe =:= defn.AnyRefType

    def isJavaApplication(t: Tree): Boolean = t match
      case Apply(f, _) => f.symbol.is(JavaDefined) && !defn.ObjectClass.isSubClass(f.symbol.owner)
      case _ => false

    def checkInterestingShapes(t: Tree): Boolean = t match
      case If(_, thenpart, elsepart) => checkInterestingShapes(thenpart) || checkInterestingShapes(elsepart)
      case Block(_, res) => checkInterestingShapes(res)
      case Match(_, cases) => cases.exists(k => checkInterestingShapes(k.body))
      case _ => checksForInterestingResult(t)

    def checksForInterestingResult(t: Tree): Boolean =
         !t.isDef                               // ignore defs
      && !isUninterestingSymbol(t.symbol)       // ctors, package, Unit, Any
      && !isUninterestingType(t.tpe)            // bottom types, Unit, Any
      && !isThisTypeResult(t)                   // buf += x
      && !isSuperConstrCall(t)                  // just a thing
      && !isJavaApplication(t)                  // Java methods are inherently side-effecting
      // && !treeInfo.hasExplicitUnit(t)           // suppressed by explicit expr: Unit // TODO Should explicit `: Unit` be added as warning suppression?

    if ctx.settings.Whas.nonUnitStatement && !ctx.isAfterTyper && checkInterestingShapes(t) then
      val where = t match
        case Block(_, res) => res
        case If(_, thenpart, Literal(Constant(()))) =>
          thenpart match {
            case Block(_, res) => res
            case _ => thenpart
          }
        case _ => t
      report.warning(UnusedNonUnitValue(where.tpe), t.srcPos)
      true
    else false
  end warnOnInterestingResultInStatement

  /** If -Wimplausible-patterns is set, warn about pattern values that can match the scrutinee
   *  type only if there would be some user-defined equality method that equates values of the
   *  two types.
   */
  def warnOnImplausiblePattern(pat: Tree, selType: Type)(using Context): Unit =
      // approximate type params with bounds
    def approx = new ApproximatingTypeMap {
      var alreadyExpanding: List[TypeRef] = Nil
      def apply(tp: Type) = tp.dealias match
        case tp: TypeRef if !tp.symbol.isClass =>
          if alreadyExpanding contains tp then tp else
            val saved = alreadyExpanding
            alreadyExpanding ::= tp
            val res = expandBounds(tp.info.bounds)
            alreadyExpanding = saved
            res
        case _ =>
          mapOver(tp)
    }

    // Is it possible that a value of `clsA` is equal to a value of `clsB`?
    //  This ignores user-defined equals methods, but makes an exception
    //  for numeric classes.
    def canOverlap(clsA: ClassSymbol, clsB: ClassSymbol): Boolean =
      clsA.mayHaveCommonChild(clsB)
      || clsA.isNumericValueClass // this is quite coarse, but matches to what was done before
      || clsB.isNumericValueClass

    // Can type `a` possiblly have a common instance with type `b`?
    def canEqual(a: Type, b: Type): Boolean = trace(i"canEqual $a $b"):
      b match
        case _: TypeRef | _: AppliedType if b.typeSymbol.isClass =>
          a match
            case a: TermRef if a.symbol.isOneOf(Module | Enum) =>
                  (a frozen_<:< b) // fast track
              || (a frozen_<:< approx(b))
            case _: TypeRef | _: AppliedType if a.typeSymbol.isClass =>
              if a.isNullType then !b.isNotNull
              else canOverlap(a.typeSymbol.asClass, b.typeSymbol.asClass)
            case a: TypeProxy =>
              canEqual(a.superType, b)
            case a: AndOrType =>
              canEqual(a.tp1, b) || canEqual(a.tp2, b)
        case b: TypeProxy =>
          canEqual(a, b.superType)
        case b: AndOrType =>
          // we lose precision with and/or types, but it's hard to do better and
          // still compute `canEqual(A & B, B & A) = true`.
          canEqual(a, b.tp1) || canEqual(a, b.tp2)

    if ctx.settings.Whas.implausiblePatterns && !canEqual(pat.tpe, selType) then
      report.warning(ImplausiblePatternWarning(pat, selType), pat.srcPos)
  end warnOnImplausiblePattern

end Linter
