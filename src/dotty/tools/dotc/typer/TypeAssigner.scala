package dotty.tools
package dotc
package typer

import core._
import ast._
import Scopes._, Contexts._, Constants._, Types._, Symbols._, Names._, Flags._, Decorators._
import ErrorReporting._, Annotations._, Denotations._, SymDenotations._, StdNames._, TypeErasure._
import TypeApplications.AppliedType
import util.Positions._
import config.Printers._
import ast.Trees._
import NameOps._
import collection.mutable

trait TypeAssigner {
  import tpd._

  /** The qualifying class of a this or super with prefix `qual` (which might be empty).
   *  @param packageOk   The qualifier may refer to a package.
   */
  def qualifyingClass(tree: untpd.Tree, qual: Name, packageOK: Boolean)(implicit ctx: Context): Symbol = {
    def qualifies(sym: Symbol) =
      sym.isClass && (
          qual.isEmpty ||
          sym.name == qual ||
          sym.is(Module) && sym.name.stripModuleClassSuffix == qual)
    ctx.outersIterator.map(_.owner).find(qualifies) match {
      case Some(c) if packageOK || !(c is Package) =>
        c
      case _ =>
        ctx.error(
          if (qual.isEmpty) tree.show + " can be used only in a class, object, or template"
          else qual.show + " is not an enclosing class", tree.pos)
        NoSymbol
    }
  }

  /** An upper approximation of the given type `tp` that does not refer to any symbol in `symsToAvoid`.
   *  Approximation steps are:
   *
   *   - follow aliases and upper bounds if the original refers to a forbidden symbol
   *   - widen termrefs that refer to a forbidden symbol
   *   - replace ClassInfos of forbidden classes by the intersection of their parents, refined by all
   *     non-private fields, methods, and type members.
   *   - if the prefix of a class refers to a forbidden symbol, first try to replace the prefix,
   *     if this is not possible, replace the ClassInfo as above.
   *   - drop refinements referring to a forbidden symbol.
   */
  def avoid(tp: Type, symsToAvoid: => List[Symbol])(implicit ctx: Context): Type = {
    val widenMap = new TypeMap {
      lazy val forbidden = symsToAvoid.toSet
      def toAvoid(tp: Type): Boolean =
        // TODO: measure the cost of using `existsPart`, and if necessary replace it
        // by a `TypeAccumulator` where we have set `stopAtStatic = true`.
        tp existsPart {
          case tp: NamedType => forbidden contains tp.symbol
          case tp: ThisType => forbidden contains tp.cls
          case _ => false
        }
      def apply(tp: Type): Type = tp match {
        case tp: TermRef if toAvoid(tp) && variance > 0 =>
          apply(tp.info.widenExpr)
        case tp: TypeRef if toAvoid(tp) =>
          tp.info match {
            case TypeAlias(ref) =>
              apply(ref)
            case info: ClassInfo if variance > 0 =>
              if (!(forbidden contains tp.symbol)) {
                val prefix = apply(tp.prefix)
                val tp1 = tp.derivedSelect(prefix)
                if (tp1.typeSymbol.exists)
                  return tp1
              }
              val parentType = info.parentsWithArgs.reduceLeft(ctx.typeComparer.andType(_, _))
              def addRefinement(parent: Type, decl: Symbol) = {
                val inherited =
                  parentType.findMember(decl.name, info.cls.thisType, Private)
                    .suchThat(decl.matches(_))
                val inheritedInfo = inherited.info
                if (inheritedInfo.exists && decl.info <:< inheritedInfo && !(inheritedInfo <:< decl.info))
                  typr.echo(
                    i"add ref $parent $decl --> ",
                    RefinedType(parent, decl.name, decl.info))
                else
                  parent
              }
              val refinableDecls = info.decls.filterNot(
                sym => sym.is(TypeParamAccessor | Private) || sym.isConstructor)
              val fullType = (parentType /: refinableDecls)(addRefinement)
              mapOver(fullType)
            case TypeBounds(lo, hi) if variance > 0 =>
              apply(hi)
            case _ =>
              mapOver(tp)
          }
        case tp @ AppliedType(tycon, args) if toAvoid(tycon) =>
          val base = apply(tycon)
          val args = tp.baseArgInfos(base.typeSymbol)
          if (base.typeParams.length == args.length) base.appliedTo(args) else base
        case tp @ RefinedType(parent, name, rinfo) if variance > 0 =>
          val parent1 = apply(tp.parent)
          val refinedInfo1 = apply(rinfo)
          if (toAvoid(refinedInfo1)) {
            typr.println(s"dropping refinement from $tp")
            parent1
          } else {
            tp.derivedRefinedType(parent1, name, refinedInfo1)
          }
        case tp: TypeVar if ctx.typerState.constraint.contains(tp) =>
          val lo = ctx.typerState.constraint.fullLowerBound(tp.origin)
          val lo1 = avoid(lo, symsToAvoid)
          if (lo1 ne lo) lo1 else tp
        case _ =>
          mapOver(tp)
      }
    }
    widenMap(tp)
  }

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
   *
   *  Also performs the following normalizations on the type `tpe`.
   *  (1) parameter accessors are alwys dereferenced.
   *  (2) if the owner of the denotation is a package object, it is assured
   *      that the package object shows up as the prefix.
   */
  def ensureAccessible(tpe: Type, superAccess: Boolean, pos: Position)(implicit ctx: Context): Type = {
    def test(tpe: Type, firstTry: Boolean): Type = tpe match {
      case tpe: NamedType =>
        val pre = tpe.prefix
        val name = tpe.name
        val d = tpe.denot.accessibleFrom(pre, superAccess)
        if (!d.exists) {
          // it could be that we found an inaccessible private member, but there is
          // an inherited non-private member with the same name and signature.
          val d2 = pre.nonPrivateMember(name)
          if (reallyExists(d2) && firstTry)
            test(tpe.shadowed.withDenot(d2), false)
          else {
            val alts = tpe.denot.alternatives.map(_.symbol).filter(_.exists)
            val what = alts match {
              case Nil =>
                name.toString
              case sym :: Nil =>
                if (sym.owner == pre.typeSymbol) sym.show else sym.showLocated
              case _ =>
                d"none of the overloaded alternatives named $name"
            }
            val where = if (ctx.owner.exists) s" from ${ctx.owner.enclosingClass}" else ""
            val whyNot = new StringBuffer
            alts foreach (_.isAccessibleFrom(pre, superAccess, whyNot))
            if (!tpe.isError)
              ctx.error(d"$what cannot be accessed as a member of $pre$where.$whyNot", pos)
            ErrorType
          }
        }
        else if (d.symbol is TypeParamAccessor)
          if (d.info.isAlias)
            ensureAccessible(d.info.bounds.hi, superAccess, pos)
          else // It's a named parameter, use the non-symbolic representation to pick up inherited versions as well
            d.symbol.owner.thisType.select(d.symbol.name)
        else
          ctx.makePackageObjPrefixExplicit(tpe withDenot d)
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
          if (name == nme.CONSTRUCTOR) d"$site does not have a constructor"
          else d"$name is not a member of $site", pos)
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

  def assignType(tree: untpd.Ident, tp: Type)(implicit ctx: Context) =
    tree.withType(tp)

  def assignType(tree: untpd.Select, qual: Tree)(implicit ctx: Context): Select = {
    def qualType = qual.tpe.widen
    def arrayElemType = {
      val JavaArrayType(elemtp) = qualType
      elemtp
    }
    val p = nme.primitive
    val tp = tree.name match {
      case p.arrayApply => MethodType(defn.IntType :: Nil, arrayElemType)
      case p.arrayUpdate => MethodType(defn.IntType :: arrayElemType :: Nil, defn.UnitType)
      case p.arrayLength => MethodType(Nil, defn.IntType)

      // Note that we do not need to handle calls to Array[T]#clone() specially:
      // The JLS section 10.7 says "The return type of the clone method of an array type
      // T[] is T[]", but the actual return type at the bytecode level is Object which
      // is casted to T[] by javac. Since the return type of Array[T]#clone() is Array[T],
      // this is exactly what Erasure will do.

      case _ => accessibleSelectionType(tree, qual)
    }
    tree.withType(tp)
  }

  def assignType(tree: untpd.SelectFromTypeTree, qual: Tree)(implicit ctx: Context) =
    tree.withType(accessibleSelectionType(tree, qual))

  def assignType(tree: untpd.New, tpt: Tree)(implicit ctx: Context) =
    tree.withType(tpt.tpe)

  def assignType(tree: untpd.Literal)(implicit ctx: Context) =
    tree.withType {
      val value = tree.const
      value.tag match {
        case UnitTag => defn.UnitType
        case NullTag => defn.NullType
        case _ => if (ctx.erasedTypes) value.tpe else ConstantType(value)
      }
    }

  def assignType(tree: untpd.This)(implicit ctx: Context) = {
    val cls = qualifyingClass(tree, tree.qual, packageOK = false)
    tree.withType(cls.thisType)
  }

  def assignType(tree: untpd.Super, qual: Tree, inConstrCall: Boolean, mixinClass: Symbol = NoSymbol)(implicit ctx: Context) = {
    val mix = tree.mix
    val qtype @ ThisType(_) = qual.tpe
    val cls = qtype.cls

    def findMixinSuper(site: Type): Type = site.parents filter (_.name == mix) match {
      case p :: Nil =>
        p
      case Nil =>
        errorType(d"$mix does not name a parent class of $cls", tree.pos)
      case p :: q :: _ =>
        errorType("ambiguous parent class qualifier", tree.pos)
    }
    val owntype =
      if (mixinClass.exists) mixinClass.typeRef
      else if (!mix.isEmpty) findMixinSuper(cls.info)
      else if (inConstrCall || ctx.erasedTypes) cls.info.firstParent
      else {
        val ps = cls.classInfo.parentsWithArgs
        if (ps.isEmpty) defn.AnyType else ps.reduceLeft((x: Type, y: Type) => x & y)
      }
    tree.withType(SuperType(cls.thisType, owntype))
  }

  def assignType(tree: untpd.Apply, fn: Tree, args: List[Tree])(implicit ctx: Context) = {
    val ownType = fn.tpe.widen match {
      case fntpe @ MethodType(_, ptypes) =>
        if (sameLength(ptypes, args) || ctx.phase.prev.relaxedTyping) fntpe.instantiate(args.tpes)
        else errorType(i"wrong number of parameters for ${fn.tpe}; expected: ${ptypes.length}", tree.pos)
      case t =>
        errorType(i"${err.exprStr(fn)} does not take parameters", tree.pos)
    }
    tree.withType(ownType)
  }

  def assignType(tree: untpd.TypeApply, fn: Tree, args: List[Tree])(implicit ctx: Context) = {
    val ownType = fn.tpe.widen match {
      case pt: PolyType =>
        val paramNames = pt.paramNames
        if (hasNamedArg(args)) {
          val argMap = new mutable.HashMap[Name, Type]
          for (NamedArg(name, arg) <- args)
            if (argMap.contains(name))
              ctx.error("duplicate name", arg.pos)
            else if (!paramNames.contains(name))
              ctx.error(s"undefined parameter name, required: ${paramNames.mkString(" or ")}", arg.pos)
            else
              argMap(name) = arg.tpe
          val gapBuf = new mutable.ListBuffer[Int]
          def nextPoly = {
            val idx = gapBuf.length
            gapBuf += idx
            PolyParam(pt, idx)
          }
          val normArgs = paramNames.map(pname => argMap.getOrElse(pname, nextPoly))
          val transform = new TypeMap {
            def apply(t: Type) = t match {
              case PolyParam(`pt`, idx) => normArgs(idx)
              case _ => mapOver(t)
            }
          }
          val resultType1 = transform(pt.resultType)
          if (gapBuf.isEmpty) resultType1
          else {
            val gaps = gapBuf.toList
            pt.derivedPolyType(
              gaps.map(paramNames.filterNot(argMap.contains)),
              gaps.map(idx => transform(pt.paramBounds(idx)).bounds),
              resultType1)
          }
        }
        else {
          val argTypes = args.tpes
          if (sameLength(argTypes, paramNames)|| ctx.phase.prev.relaxedTyping) pt.instantiate(argTypes)
          else errorType(d"wrong number of type parameters for ${fn.tpe}; expected: ${pt.paramNames.length}", tree.pos)
        }
      case _ =>
        errorType(i"${err.exprStr(fn)} does not take type parameters", tree.pos)
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
    tree.withType(avoid(expr.tpe, localSyms(stats) filter (_.isTerm)))

  def assignType(tree: untpd.If, thenp: Tree, elsep: Tree)(implicit ctx: Context) =
    tree.withType(thenp.tpe | elsep.tpe)

  def assignType(tree: untpd.Closure, meth: Tree, target: Tree)(implicit ctx: Context) =
    tree.withType(
        if (target.isEmpty) meth.tpe.widen.toFunctionType(tree.env.length)
        else target.tpe)

  def assignType(tree: untpd.CaseDef, body: Tree)(implicit ctx: Context) =
    tree.withType(body.tpe)

  def assignType(tree: untpd.Match, cases: List[CaseDef])(implicit ctx: Context) =
    tree.withType(ctx.typeComparer.lub(cases.tpes))

  def assignType(tree: untpd.Return)(implicit ctx: Context) =
    tree.withType(defn.NothingType)

  def assignType(tree: untpd.Try, expr: Tree, cases: List[CaseDef])(implicit ctx: Context) =
    if (cases.isEmpty) tree.withType(expr.tpe)
    else tree.withType(ctx.typeComparer.lub(expr.tpe :: cases.tpes))

  def assignType(tree: untpd.SeqLiteral, elems: List[Tree], elemtpt: Tree)(implicit ctx: Context) = {
    val ownType = tree match {
      case tree: JavaSeqLiteral => defn.ArrayOf(elemtpt.tpe)
      case _ => if (ctx.erasedTypes) defn.SeqType else defn.SeqType.appliedTo(elemtpt.tpe)
    }
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
    lazy val ntparams = tycon.tpe.namedTypeParams
    def refineNamed(tycon: Type, arg: Tree) = arg match {
      case ast.Trees.NamedArg(name, argtpt) =>
        // Dotty deviation: importing ast.Trees._ and matching on NamedArg gives a cyclic ref error
        val tparam = tparams.find(_.name == name) match {
          case Some(tparam) => tparam
          case none => ntparams.find(_.name == name).getOrElse(NoSymbol)
        }
        if (tparam.exists) RefinedType(tycon, name, argtpt.tpe.toBounds(tparam))
        else errorType(i"$tycon does not have a parameter or abstract type member named $name", arg.pos)
      case _ =>
        errorType(s"named and positional type arguments may not be mixed", arg.pos)
    }
    val ownType =
      if (hasNamedArg(args)) (tycon.tpe /: args)(refineNamed)
      else if (sameLength(tparams, args)) tycon.tpe.appliedTo(args.tpes)
      else errorType(d"wrong number of type arguments for ${tycon.tpe}, should be ${tparams.length}", tree.pos)
    tree.withType(ownType)
  }

  def assignType(tree: untpd.ByNameTypeTree, result: Tree)(implicit ctx: Context) =
    tree.withType(ExprType(result.tpe))

  def assignType(tree: untpd.TypeBoundsTree, lo: Tree, hi: Tree)(implicit ctx: Context) =
    tree.withType(if (lo eq hi) TypeAlias(lo.tpe) else TypeBounds(lo.tpe, hi.tpe))

  def assignType(tree: untpd.Bind, sym: Symbol)(implicit ctx: Context) =
    tree.withType(NamedType.withFixedSym(NoPrefix, sym))

  def assignType(tree: untpd.Alternative, trees: List[Tree])(implicit ctx: Context) =
    tree.withType(ctx.typeComparer.lub(trees.tpes))

  def assignType(tree: untpd.UnApply, proto: Type)(implicit ctx: Context) =
    tree.withType(proto)

  def assignType(tree: untpd.ValDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(if (sym.exists) assertExists(symbolicIfNeeded(sym).orElse(sym.valRef)) else NoType)

  def assignType(tree: untpd.DefDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(symbolicIfNeeded(sym).orElse(sym.termRefWithSig))

  def assignType(tree: untpd.TypeDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(symbolicIfNeeded(sym).orElse(sym.typeRef))

  private def symbolicIfNeeded(sym: Symbol)(implicit ctx: Context) = {
    val owner = sym.owner
    owner.infoOrCompleter match {
      case info: ClassInfo if info.givenSelfType.exists =>
        // In that case a simple typeRef/termWithWithSig could return a member of
        // the self type, not the symbol itself. To avoid this, we make the reference
        // symbolic. In general it seems to be faster to keep the non-symblic
        // reference, since there is less pressure on the uniqueness tables that way
        // and less work to update all the different references. That's why symbolic references
        // are only used if necessary.
        NamedType.withFixedSym(owner.thisType, sym)
      case _ => NoType
    }
  }

  def assertExists(tp: Type) = { assert(tp != NoType); tp }

  def assignType(tree: untpd.Import, sym: Symbol)(implicit ctx: Context) =
    tree.withType(sym.nonMemberTermRef)

  def assignType(tree: untpd.Annotated, annot: Tree, arg: Tree)(implicit ctx: Context) =
    tree.withType(AnnotatedType(arg.tpe, Annotation(annot)))

  def assignType(tree: untpd.PackageDef, pid: Tree)(implicit ctx: Context) =
    tree.withType(pid.symbol.valRef)
}

object TypeAssigner extends TypeAssigner

