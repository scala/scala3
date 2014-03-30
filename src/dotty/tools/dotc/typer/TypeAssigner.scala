package dotty.tools
package dotc
package typer

import core._
import ast._
import Scopes._, Contexts._, Constants._, Types._, Symbols._, Names._, Flags._, Decorators._
import ErrorReporting._, Annotations._, Denotations._, SymDenotations._, StdNames._
import util.Positions._
import config.Printers._

trait TypeAssigner {
  import tpd._

  /** The enclosing class, except if we are in a super call, in which case
   *  it is the next outer one.
   */
  def effectiveEnclosingClass(implicit ctx: Context) = {
    val enclClass = ctx.owner.enclosingClass
    if ((ctx.mode is Mode.InSuperCall) && enclClass.exists) enclClass.owner.enclosingClass
    else enclClass
  }

  /** The qualifying class of a this or super with prefix `qual` (which might be empty).
   *  @param packageOk   The qualifier may refer to a package.
   */
  def qualifyingClass(tree: untpd.Tree, qual: Name, packageOK: Boolean)(implicit ctx: Context): Symbol = {
    effectiveEnclosingClass.ownersIterator.find(o => qual.isEmpty || o.isClass && o.name == qual) match {
      case Some(c) if packageOK || !(c is Package) =>
        c
      case _ =>
        ctx.error(
          if (qual.isEmpty) tree.show + " can be used only in a class, object, or template"
          else qual.show + " is not an enclosing class", tree.pos)
        NoSymbol
    }
  }

  def avoid(tp: Type, syms: => List[Symbol])(implicit ctx: Context): Type = {
    val widenMap = new TypeMap {
      lazy val forbidden = syms.toSet
      def toAvoid(tp: Type): Boolean = tp match {
        case tp: TermRef =>
          val sym = tp.symbol
          sym.exists && (
               sym.owner.isTerm && (forbidden contains sym)
            || !(sym.owner is Package) && toAvoid(tp.prefix)
            )
        case _ =>
          false
      }
      def apply(tp: Type) = tp match {
        case tp: TermRef if toAvoid(tp) && variance > 0 =>
          apply(tp.info)
        case tp: TypeRef if toAvoid(tp.prefix) =>
          tp.info match {
            case TypeAlias(ref) => apply(ref)
            case _ => mapOver(tp)
          }
        case tp: RefinedType =>
          val tp1 @ RefinedType(parent1, _) = mapOver(tp)
          if (tp1.refinedInfo existsPart toAvoid) {
            typr.println(s"dropping refinement from $tp1")
            parent1
          }
          else tp1
        case _ =>
          mapOver(tp)
      }
    }
    widenMap(tp)
  }

  def localSyms(stats: List[tpd.Tree])(implicit ctx: Context): List[Symbol] =
    for (stat <- stats if stat.isDef) yield stat.symbol

  def seqToRepeated(tree: Tree)(implicit ctx: Context): Tree =
    Typed(tree, TypeTree(tree.tpe.widen.translateParameterized(defn.SeqClass, defn.RepeatedParamClass)))

  /** A denotation exists really if it exists and does not point to a stale symbol. */
  final def reallyExists(denot: Denotation)(implicit ctx: Context): Boolean = try
    denot match {
      case denot: SymDenotation =>
        denot.exists && {
          denot.ensureCompleted
          !denot.isAbsent
        }
      case denot: SingleDenotation =>
        val sym = denot.symbol
        (sym eq NoSymbol) || reallyExists(sym.denot)
      case _ =>
        true
    }
  catch {
    case ex: StaleSymbol => false
  }

  /** If `tpe` is a named type, check that its denotation is accessible in the
   *  current context. Return the type with those alternatives as denotations
   *  which are accessible.
   */
  def ensureAccessible(tpe: Type, superAccess: Boolean, pos: Position)(implicit ctx: Context): Type = {
    def test(tpe: Type, firstTry: Boolean): Type = tpe match {
      case tpe: NamedType =>
        val pre = tpe.prefix
        val name = tpe.name
        val d = tpe.denot.accessibleFrom(pre, superAccess)
        if (!d.exists) {
          // it could be that we found an inaccessbile private member, but there is
          // an inherited non-private member with the same name and signature.
          val d2 = pre.nonPrivateMember(name)
          if (reallyExists(d2) && firstTry) test(pre.select(name, d2), false)
          else {
            val alts = tpe.denot.alternatives.map(_.symbol).filter(_.exists)
            val what = alts match {
              case Nil =>
                name.toString
              case sym :: Nil =>
                if (sym.owner == pre.typeSymbol) sym.show else sym.showLocated
              case _ =>
                i"none of the overloaded alternatives named $name"
            }
            val where = if (ctx.owner.exists) s" from ${ctx.owner.enclosingClass}" else ""
            val whyNot = new StringBuffer
            alts foreach (_.isAccessibleFrom(pre, superAccess, whyNot))
            if (!tpe.isError)
              ctx.error(i"$what cannot be accessed as a member of $pre$where.$whyNot", pos)
            ErrorType
          }
        } else if (d.symbol is TypeParamAccessor) // always dereference type param accessors
          ensureAccessible(d.info.bounds.hi, superAccess, pos)
        else
          tpe withDenot d
      case _ =>
        tpe
    }
    test(tpe, true)
  }

  /** The type of a selection with `name` of a tree with type `site`.
   */
  def selectionType(site: Type, name: Name, pos: Position)(implicit ctx: Context): Type = {
    val mbr = site.member(name)
    if (reallyExists(mbr)) site.select(name, mbr)
    else {
      if (!site.isErroneous) {
        ctx.error(
          if (name == nme.CONSTRUCTOR) i"$site does not have a constructor"
          else i"$name is not a member of $site", pos)
      }
      ErrorType
    }
  }

  /** The selection type, which is additionally checked for accessibility.
   */
  def accessibleSelectionType(tree: untpd.RefTree, qual1: Tree)(implicit ctx: Context): Type = {
    val ownType = selectionType(qual1.tpe.widenIfUnstable, tree.name, tree.pos)
    ensureAccessible(ownType, qual1.isInstanceOf[Super], tree.pos)
  }

  /** Type assignment method. Each method takes as parameters
   *   - an untpd.Tree to which it assigns a type,
   *   - typed child trees it needs to access to cpmpute that type,
   *   - any further information it needs to access to compute that type.
   */

  def assignType(tree: untpd.Ident, rawType: Type)(implicit ctx: Context) = {
    tree.withType(if (tree.isType) rawType else rawType.underlyingIfRepeated)
  }

  def assignType(tree: untpd.Select, qual: Tree)(implicit ctx: Context) = {
    tree.withType(accessibleSelectionType(tree, qual))
  }

  def assignType(tree: untpd.SelectFromTypeTree, qual: Tree)(implicit ctx: Context) = {
    tree.withType(accessibleSelectionType(tree, qual))
  }

  def assignType(tree: untpd.New, tpt: Tree)(implicit ctx: Context) =
    tree.withType(tpt.tpe)

  def assignType(tree: untpd.Literal)(implicit ctx: Context) =
    tree.withType {
      tree.const.tag match {
        case UnitTag => defn.UnitType
        case NullTag => defn.NullType
        case _ => ConstantType(tree.const)
      }
    }

  def assignType(tree: untpd.This)(implicit ctx: Context) = {
    val cls = qualifyingClass(tree, tree.qual, packageOK = false)
    tree.withType(cls.thisType)
  }

  def assignType(tree: untpd.Super, qual: Tree, inConstrCall: Boolean)(implicit ctx: Context) = {
    val mix = tree.mix
    val cls = qual.tpe.widen.typeSymbol

    def findMixinSuper(site: Type): Type = site.parents filter (_.name == mix) match {
      case p :: Nil =>
        p
      case Nil =>
        errorType(i"$mix does not name a parent class of $cls", tree.pos)
      case p :: q :: _ =>
        errorType(s"ambiguous parent class qualifier", tree.pos)
    }
    val owntype =
      if (!mix.isEmpty) findMixinSuper(cls.info)
      else if (inConstrCall) cls.info.firstParent
      else cls.info.parents.reduceLeft((x: Type, y: Type) => AndType(x, y))
    tree.withType(SuperType(cls.thisType, owntype))
  }

  def assignType(tree: untpd.Apply, fn: Tree, args: List[Tree])(implicit ctx: Context) = {
    val ownType = fn.tpe.widen match {
      case fntpe @ MethodType(_, ptypes) =>
        if (sameLength(ptypes, args)) fntpe.instantiate(args.tpes)
        else errorType(s"wrong number of parameters for ${fn.tpe}; expected: ${ptypes.length}", tree.pos)
      case t =>
        errorType(s"${err.exprStr(fn)} does not take parameters", tree.pos)
    }
    tree.withType(ownType)
  }

  def assignType(tree: untpd.TypeApply, fn: Tree, args: List[Tree])(implicit ctx: Context) = {
    val ownType = fn.tpe.widen match {
      case pt: PolyType =>
        val argTypes = args.tpes
        if (sameLength(argTypes, pt.paramNames)) pt.instantiate(argTypes)
        else errorType(i"wrong number of type parameters for ${fn.tpe}; expected: ${pt.paramNames.length}", tree.pos)
      case _ =>
        errorType(s"${err.exprStr(fn)} does not take type parameters", tree.pos)
    }
    tree.withType(ownType)
  }

  def assignType(tree: untpd.Pair, left: Tree, right: Tree)(implicit ctx: Context) =
    tree.withType(defn.PairType.appliedTo(left.tpe :: right.tpe :: Nil))

  def assignType(tree: untpd.Typed, tpt: Tree)(implicit ctx: Context) =
    tree.withType(tpt.tpe)

  def assignType(tree: untpd.NamedArg, arg: Tree)(implicit ctx: Context) =
    tree.withType(arg.tpe)

  def assignType(tree: untpd.Assign)(implicit ctx: Context) =
    tree.withType(defn.UnitType)

  def assignType(tree: untpd.Block, stats: List[Tree], expr: Tree)(implicit ctx: Context) =
    tree.withType(avoid(expr.tpe, localSyms(stats)))

  def assignType(tree: untpd.If, thenp: Tree, elsep: Tree)(implicit ctx: Context) =
    tree.withType(thenp.tpe | elsep.tpe)

  def assignType(tree: untpd.Closure, meth: Tree, target: Tree)(implicit ctx: Context) =
    tree.withType(if (target.isEmpty) meth.tpe.widen.toFunctionType else target.tpe)

  def assignType(tree: untpd.CaseDef, body: Tree)(implicit ctx: Context) =
    tree.withType(body.tpe)

  def assignType(tree: untpd.Match, cases: List[CaseDef])(implicit ctx: Context) =
    tree.withType(ctx.typeComparer.lub(cases.tpes))

  def assignType(tree: untpd.Return)(implicit ctx: Context) =
    tree.withType(defn.NothingType)

  def assignType(tree: untpd.Try, expr: Tree, handler: Tree)(implicit ctx: Context) = {
    val handlerTypeArgs = handler.tpe.baseArgTypesHi(defn.FunctionClass(1))
    tree.withType(if (handlerTypeArgs.nonEmpty) expr.tpe | handlerTypeArgs(1) else expr.tpe)
  }

  def assignType(tree: untpd.Throw)(implicit ctx: Context) =
    tree.withType(defn.NothingType)

  def assignType(tree: untpd.SeqLiteral, elems: List[Tree])(implicit ctx: Context) = {
    val ownType =
      if (ctx.erasedTypes) defn.SeqType
      else defn.SeqType.appliedTo(ctx.typeComparer.lub(elems.tpes))
    tree.withType(ownType)
  }

  def assignType(tree: untpd.SingletonTypeTree, ref: Tree)(implicit ctx: Context) =
    tree.withType(ref.tpe)

  def assignType(tree: untpd.AndTypeTree, left: Tree, right: Tree)(implicit ctx: Context) =
    tree.withType(left.tpe & right.tpe)

  def assignType(tree: untpd.OrTypeTree, left: Tree, right: Tree)(implicit ctx: Context) =
    tree.withType(left.tpe | right.tpe)

  // RefinedTypeTree is missing, handled specially in Typer and Unpickler.

  def assignType(tree: untpd.AppliedTypeTree, tycon: Tree, args: List[Tree])(implicit ctx: Context) = {
    val tparams = tycon.tpe.typeParams
    val ownType =
      if (sameLength(tparams, args)) tycon.tpe.appliedTo(args.tpes)
      else errorType(i"wrong number of type arguments for ${tycon.tpe}, should be ${tparams.length}", tree.pos)
    tree.withType(ownType)
  }

  def assignType(tree: untpd.ByNameTypeTree, result: Tree)(implicit ctx: Context) =
    tree.withType(ExprType(result.tpe))

  def assignType(tree: untpd.TypeBoundsTree, lo: Tree, hi: Tree)(implicit ctx: Context) =
    tree.withType(TypeBounds(lo.tpe, hi.tpe))

  def assignType(tree: untpd.Bind, sym: TermSymbol)(implicit ctx: Context) =
    tree.withType(TermRef(NoPrefix, sym))

  def assignType(tree: untpd.Alternative, trees: List[Tree])(implicit ctx: Context) =
    tree.withType(ctx.typeComparer.lub(trees.tpes))

  def assignType(tree: untpd.UnApply, proto: Type)(implicit ctx: Context) =
    tree.withType(proto)

  def assignType(tree: untpd.ValDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(if (sym.exists) sym.valRef else NoType)

  def assignType(tree: untpd.DefDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(sym.termRefWithSig)

  def assignType(tree: untpd.TypeDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(sym.typeRef)

  def assignType(tree: untpd.Import, sym: Symbol)(implicit ctx: Context) =
    tree.withType(sym.termRef)

  def assignType(tree: untpd.Annotated, annot: Tree, arg: Tree)(implicit ctx: Context) =
    tree.withType(AnnotatedType(Annotation(annot), arg.tpe))

  def assignType(tree: untpd.PackageDef, pid: Tree)(implicit ctx: Context) =
    tree.withType(pid.symbol.valRef)
}

object TypeAssigner extends TypeAssigner

