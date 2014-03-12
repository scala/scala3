package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Types._, Flags._, Denotations._, Names._, StdNames._, NameOps._, Symbols._
import Trees._, ProtoTypes._
import Constants._
import Scopes._
import annotation.unchecked
import util.Positions._
import util.{Stats, SimpleMap}
import util.common._
import Decorators._
import Uniques._
import ErrorReporting.{errorType, InfoString}
import config.Printers._
import collection.mutable

trait NoChecking {
  import tpd._
  def checkValue(tree: Tree, proto: Type)(implicit ctx: Context): tree.type = tree
  def checkBounds(args: List[tpd.Tree], poly: PolyType, pos: Position)(implicit ctx: Context): Unit = ()
  def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Unit = ()
  def checkLegalPrefix(tp: Type, pos: Position)(implicit ctx: Context): Unit = ()
  def checkClassTypeWithStablePrefix(tp: Type, pos: Position, traitReq: Boolean)(implicit ctx: Context): Type = tp
  def checkImplicitTptNonEmpty(defTree: untpd.ValOrDefDef)(implicit ctx: Context): Unit = ()
  def checkImplicitParamsNotSingletons(vparamss: List[List[ValDef]])(implicit ctx: Context): Unit = ()
  def checkFeasible(tp: Type, pos: Position, where: => String = "")(implicit ctx: Context): Type = tp
  def checkNoDoubleDefs(cls: Symbol)(implicit ctx: Context): Unit = ()
}

trait Checking extends NoChecking {

  import tpd._

  /** Check that Java statics and packages can only be used in selections.
   */
  override def checkValue(tree: Tree, proto: Type)(implicit ctx: Context): tree.type = {
    if (!proto.isInstanceOf[SelectionProto]) {
      val sym = tree.tpe.termSymbol
      if ((sym is Package) || (sym is JavaModule)) ctx.error(i"$sym is not a value", tree.pos)
    }
    tree
  }

  /** Check that type arguments `args` conform to corresponding bounds in `poly` */
  override def checkBounds(args: List[tpd.Tree], poly: PolyType, pos: Position)(implicit ctx: Context): Unit = {
    val argTypes = args.tpes
    def substituted(tp: Type) = tp.substParams(poly, argTypes)
    for ((arg, bounds) <- args zip poly.paramBounds) {
      def notConforms(which: String, bound: Type) =
        ctx.error(i"Type argument ${arg.tpe} does not conform to $which bound $bound", arg.pos)
      if (!(arg.tpe <:< substituted(bounds.hi))) notConforms("upper", bounds.hi)
      if (!(bounds.lo <:< arg.tpe)) notConforms("lower", bounds.lo)
    }
  }

  /** Check that type `tp` is stable. */
  override def checkStable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    if (!tp.isStable) ctx.error(i"$tp is not stable", pos)

  /** Check that type `tp` is a legal prefix for '#'.
   *  @return The type itself
   */
  override def checkLegalPrefix(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    if (!tp.isLegalPrefix) ctx.error(i"$tp is not a valid prefix for '#'", pos)

 /** Check that `tp` is a class type with a stable prefix. Also, if `isFirst` is
   *  false check that `tp` is a trait.
   *  @return  `tp` itself if it is a class or trait ref, ObjectClass.typeRef if not.
   */
  override def checkClassTypeWithStablePrefix(tp: Type, pos: Position, traitReq: Boolean)(implicit ctx: Context): Type =
    tp.underlyingClassRef match {
      case tref: TypeRef =>
        checkStable(tref.prefix, pos)
        if (traitReq && !(tref.symbol is Trait)) ctx.error(i"$tref is not a trait", pos)
        tp
    case _ =>
      ctx.error(i"$tp is not a class type", pos)
      defn.ObjectClass.typeRef
  }

  /** Check that (return) type of implicit definition is not empty */
  override def checkImplicitTptNonEmpty(defTree: untpd.ValOrDefDef)(implicit ctx: Context): Unit = defTree.tpt match {
    case TypeTree(original) if original.isEmpty =>
      val resStr = if (defTree.isInstanceOf[untpd.DefDef]) "result " else ""
      ctx.error(i"${resStr}type of implicit definition needs to be given explicitly", defTree.pos)
    case _ =>
  }

  /** Check that a non-implicit parameter making up the first parameter section of an
   *  implicit conversion is not a singleton type.
   */
  override def checkImplicitParamsNotSingletons(vparamss: List[List[ValDef]])(implicit ctx: Context): Unit = vparamss match {
    case (vparam :: Nil) :: _ if !(vparam.symbol is Implicit) =>
      if (vparam.tpt.tpe.isInstanceOf[SingletonType])
        ctx.error(s"implicit conversion may not have a parameter of singleton type", vparam.tpt.pos)
    case _ =>
  }

  /** Check that any top-level type arguments in this type are feasible, i.e. that
   *  their lower bound conforms to their upper cound. If a type argument is
   *  infeasible, issue and error and continue with upper bound.
   */
  override def checkFeasible(tp: Type, pos: Position, where: => String = "")(implicit ctx: Context): Type = tp match {
    case tp: RefinedType =>
      tp.derivedRefinedType(tp.parent, tp.refinedName, checkFeasible(tp.refinedInfo, pos, where))
    case tp @ TypeBounds(lo, hi) if !(lo <:< hi) =>
      ctx.error(i"no type exists between low bound $lo and high bound $hi$where", pos)
      tp.derivedTypeAlias(hi)
    case _ =>
      tp
  }

  /** Check that class does not define */
  override def checkNoDoubleDefs(cls: Symbol)(implicit ctx: Context): Unit = {
    val seen = new mutable.HashMap[Name, List[Symbol]] {
      override def default(key: Name) = Nil
    }
    typr.println(i"check no double defs $cls")
    for (decl <- cls.info.decls) {
      for (other <- seen(decl.name)) {
        typr.println(i"conflict? $decl $other")
        if (decl.signature matches other.signature) {
          def doubleDefError(decl: Symbol, other: Symbol): Unit = {
            def ofType = if (decl.isType) "" else i": ${other.info}"
            def explanation =
              if (!decl.isSourceMethod) ""
              else "\n (both definitions have the same erased type signature)"
            ctx.error(i"$decl is already defined as $other$ofType$explanation", decl.pos)
          }
          if (decl is Synthetic) doubleDefError(other, decl)
          else doubleDefError(decl, other)
        }
        if ((decl is HasDefaultParams) && (other is HasDefaultParams)) {
          ctx.error(i"two or more overloaded variants of $decl have default arguments")
          decl resetFlag HasDefaultParams
        }
      }
      seen(decl.name) = decl :: seen(decl.name)
    }
  }

  def checkInstantiatable(cls: ClassSymbol, pos: Position): Unit = {
    ??? // to be done in later phase: check that class `cls` is legal in a new.
  }
}