package dottyBench.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Constants._, Types._, Symbols._, Names._, Flags._, Decorators._
import ErrorReporting._, Annotations._, Denotations._, SymDenotations._, StdNames._
import util.Spans._
import util.SourcePosition
import config.Printers.typr
import ast.Trees._
import NameOps._
import ProtoTypes._
import collection.mutable
import reporting._
import Checking.{checkNoPrivateLeaks, checkNoWildcard}

trait TypeAssigner {
  import tpd._

  /** The qualifying class of a this or super with prefix `qual` (which might be empty).
   *  @param packageOk   The qualifier may refer to a package.
   */
  def qualifyingClass(tree: untpd.Tree, qual: Name, packageOK: Boolean)(using Ctx, CState): Symbol = {
    def qualifies(sym: Symbol) =
      sym.isClass && (
          qual.isEmpty ||
          sym.name == qual ||
          sym.is(Module) && sym.name.stripModuleClassSuffix == qual)
    ctx.outersIterator.map(_.owner).find(qualifies) match {
      case Some(c) if packageOK || !c.is(Package) =>
        c
      case _ =>
        report.error(
          if (qual.isEmpty) tree.show + " can be used only in a class, object, or template"
          else qual.show + " is not an enclosing class", tree.sourcePos)
        NoSymbol
    }
  }

  def avoidingType(expr: Tree, bindings: List[Tree])(using Ctx, CState): Type =
    TypeOps.avoid(expr.tpe, localSyms(bindings).filter(_.isTerm))

  def avoidPrivateLeaks(sym: Symbol)(using Ctx, CState): Type =
    if sym.owner.isClass && !sym.isOneOf(JavaOrPrivateOrSynthetic)
    then checkNoPrivateLeaks(sym)
    else sym.info

  private def toRepeated(tree: Tree, from: ClassSymbol)(using Ctx, CState): Tree =
    Typed(tree, TypeTree(tree.tpe.widen.translateToRepeated(from)))

  def seqToRepeated(tree: Tree)(using Ctx, CState): Tree = toRepeated(tree, defn.SeqClass)

  def arrayToRepeated(tree: Tree)(using Ctx, CState): Tree = toRepeated(tree, defn.ArrayClass)

  /** A denotation exists really if it exists and does not point to a stale symbol. */
  final def reallyExists(denot: Denotation)(using Ctx, CState): Boolean = try
    denot match {
      case denot: SymDenotation =>
        denot.exists && !denot.isAbsent()
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
   *  (1) parameter accessors are always dereferenced.
   *  (2) if the owner of the denotation is a package object, it is assured
   *      that the package object shows up as the prefix.
   */
  def ensureAccessible(tpe: Type, superAccess: Boolean, pos: SourcePosition)(using Ctx, CState): Type = {
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
            test(NamedType(pre, name, d2), false)
          else if (pre.derivesFrom(defn.DynamicClass) && name.isTermName)
            TryDynamicCallType
          else {
            val alts = tpe.denot.alternatives.map(_.symbol).filter(_.exists)
            var packageAccess = false
            val whatCanNot = alts match {
              case Nil =>
                em"$name cannot"
              case sym :: Nil =>
                em"${if (sym.owner == pre.typeSymbol) sym.show else sym.showLocated} cannot"
              case _ =>
                em"none of the overloaded alternatives named $name can"
            }
            val where = if (ctx.owner.exists) s" from ${ctx.owner.enclosingClass}" else ""
            val whyNot = new StringBuffer
            alts foreach (_.isAccessibleFrom(pre, superAccess, whyNot))
            if (tpe.isError) tpe
            else errorType(ex"$whatCanNot be accessed as a member of $pre$where.$whyNot", pos)
          }
        }
        else TypeOps.makePackageObjPrefixExplicit(tpe withDenot d)
      case _ =>
        tpe
    }
    test(tpe, true)
  }

  /** Return a potentially skolemized version of `qualTpe` to be used
   *  as a prefix when selecting `name`.
   *
   *  @see QualSkolemType, TypeOps#asSeenFrom
   */
  def maybeSkolemizePrefix(qualType: Type, name: Name)(using Ctx, CState): Type =
    if (name.isTermName && !TypeOps.isLegalPrefix(qualType))
      QualSkolemType(qualType)
    else
      qualType

  /** The type of the selection `tree`, where `qual1` is the typed qualifier part. */
  def selectionType(tree: untpd.RefTree, qual1: Tree)(using Ctx, CState): Type = {
    var qualType = qual1.tpe.widenIfUnstable
    if (!qualType.hasSimpleKind && tree.name != nme.CONSTRUCTOR)
      // constructors are selected on typeconstructor, type arguments are passed afterwards
      qualType = errorType(em"$qualType takes type parameters", qual1.sourcePos)
    else if (!qualType.isInstanceOf[TermType])
      qualType = errorType(em"$qualType is illegal as a selection prefix", qual1.sourcePos)

    val name = tree.name
    val pre = maybeSkolemizePrefix(qualType, name)
    val mbr = qualType.findMember(name, pre)
    def isDynamicExpansion(tree: untpd.RefTree): Boolean = {
      Dynamic.isDynamicMethod(name) || (
        tree match
          case Select(Apply(fun: untpd.RefTree, _), nme.apply) if defn.isContextFunctionClass(fun.symbol.owner) =>
            isDynamicExpansion(fun)
          case  Select(qual, nme.apply) =>
            Dynamic.isDynamicMethod(qual.symbol.name) && tree.span.isSynthetic
          case _ => false
      )
    }
    if (reallyExists(mbr))
      qualType.select(name, mbr)
    else if (qualType.derivesFrom(defn.DynamicClass) && name.isTermName && !isDynamicExpansion(tree))
      TryDynamicCallType
    else if (qualType.isErroneous || name.toTermName == nme.ERROR)
      UnspecifiedErrorType
    else if (name == nme.CONSTRUCTOR)
      errorType(ex"$qualType does not have a constructor", tree.sourcePos)
    else {
      val kind = if (name.isTypeName) "type" else "value"
      def addendum = err.selectErrorAddendum(tree, qual1, qualType, importSuggestionAddendum)
      errorType(NotAMember(qualType, name, kind, addendum), tree.sourcePos)
    }
  }

  def importSuggestionAddendum(pt: Type)(using Ctx, CState): String = ""

  /** The type of the selection in `tree`, where `qual1` is the typed qualifier part.
   *  The selection type is additionally checked for accessibility.
   */
  def accessibleSelectionType(tree: untpd.RefTree, qual1: Tree)(using Ctx, CState): Type = {
    val ownType = selectionType(tree, qual1)
    if (tree.hasAttachment(desugar.SuppressAccessCheck)) ownType
    else ensureAccessible(ownType, qual1.isInstanceOf[Super], tree.sourcePos)
  }

  /** Type assignment method. Each method takes as parameters
   *   - an untpd.Tree to which it assigns a type,
   *   - typed child trees it needs to access to cpmpute that type,
   *   - any further information it needs to access to compute that type.
   */
  def assignType(tree: untpd.Ident, tp: Type)(using Ctx, CState): Ident =
    tree.withType(tp)

  def assignType(tree: untpd.Select, qual: Tree)(using Ctx, CState): Select = {
    def qualType = qual.tpe.widen
    def arrayElemType = {
      qualType match {
        case JavaArrayType(elemtp) => elemtp
        case _ =>
          report.error("Expected Array but was " + qualType.show, tree.sourcePos)
          defn.NothingType
      }
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
    ConstFold(tree.withType(tp))
  }

  /** Normalize type T appearing in a new T by following eta expansions to
   *  avoid higher-kinded types.
   */
  def typeOfNew(tpt: Tree)(using Ctx, CState): Type = tpt.tpe.dealias match {
    case TypeApplications.EtaExpansion(tycon) => tycon
    case t => tpt.tpe
  }

  def assignType(tree: untpd.New, tpt: Tree)(using Ctx, CState): New =
    tree.withType(typeOfNew(tpt))

  def assignType(tree: untpd.Literal)(using Ctx, CState): Literal =
    tree.withType {
      val value = tree.const
      value.tag match {
        case UnitTag => defn.UnitType
        case NullTag => defn.NullType
        case _ => if (currentlyAfterErasure) value.tpe else ConstantType(value)
      }
    }

  def assignType(tree: untpd.This)(using Ctx, CState): This = {
    val cls = qualifyingClass(tree, tree.qual.name, packageOK = false)
    tree.withType(
        if (cls.isClass) cls.thisType
        else errorType("not a legal qualifying class for this", tree.sourcePos))
  }

  def assignType(tree: untpd.Super, qual: Tree, mixinClass: Symbol = NoSymbol)(using Ctx, CState): Super = {
    val mix = tree.mix
    qual.tpe match {
      case err: ErrorType => untpd.cpy.Super(tree)(qual, mix).withType(err)
      case qtype @ ThisType(_) =>
        val cls = qtype.cls
        def findMixinSuper(site: Type): Type = site.parents filter (_.typeSymbol.name == mix.name) match {
          case p :: Nil =>
            p.typeConstructor
          case Nil =>
            errorType(SuperQualMustBeParent(mix, cls), tree.sourcePos)
          case p :: q :: _ =>
            errorType("ambiguous parent class qualifier", tree.sourcePos)
        }
        val owntype =
          if (mixinClass.exists) mixinClass.appliedRef
          else if (!mix.isEmpty) findMixinSuper(cls.info)
          else if (currentlyAfterErasure) cls.info.firstParent.typeConstructor
          else {
            val ps = cls.classInfo.parents
            if (ps.isEmpty) defn.AnyType else ps.reduceLeft((x: Type, y: Type) => x & y)
          }
        tree.withType(SuperType(cls.thisType, owntype))
    }
  }

  /** Substitute argument type `argType` for parameter `pref` in type `tp`,
   *  skolemizing the argument type if it is not stable and `pref` occurs in `tp`.
   */
  def safeSubstParam(tp: Type, pref: ParamRef, argType: Type)(using Ctx, CState): Type = {
    val tp1 = tp.substParam(pref, argType)
    if ((tp1 eq tp) || argType.isStable) tp1
    else tp.substParam(pref, SkolemType(argType.widen))
  }

  /** Substitute types of all arguments `args` for corresponding `params` in `tp`.
   *  The number of parameters `params` may exceed the number of arguments.
   *  In this case, only the common prefix is substituted.
   */
  def safeSubstParams(tp: Type, params: List[ParamRef], argTypes: List[Type])(using Ctx, CState): Type = argTypes match {
    case argType :: argTypes1 =>
      val tp1 = safeSubstParam(tp, params.head, argType)
      safeSubstParams(tp1, params.tail, argTypes1)
    case Nil =>
      tp
  }

  def assignType(tree: untpd.Apply, fn: Tree, args: List[Tree])(using Ctx, CState): Apply = {
    val ownType = fn.tpe.widen match {
      case fntpe: MethodType =>
        if (sameLength(fntpe.paramInfos, args) || currentPhase.prev.relaxedTyping)
          if (fntpe.isResultDependent) safeSubstParams(fntpe.resultType, fntpe.paramRefs, args.tpes)
          else fntpe.resultType
        else
          errorType(i"wrong number of arguments at ${currentPhase.prev} for $fntpe: ${fn.tpe}, expected: ${fntpe.paramInfos.length}, found: ${args.length}", tree.sourcePos)
      case t =>
        if (ctx.settings.Ydebug.value) new FatalError("").printStackTrace()
        errorType(err.takesNoParamsStr(fn, ""), tree.sourcePos)
    }
    ConstFold(tree.withType(ownType))
  }

  def assignType(tree: untpd.TypeApply, fn: Tree, args: List[Tree])(using Ctx, CState): TypeApply = {
    def fail = tree.withType(errorType(err.takesNoParamsStr(fn, "type "), tree.sourcePos))
    ConstFold(fn.tpe.widen match {
      case pt: TypeLambda =>
        tree.withType {
          val paramNames = pt.paramNames
          if (hasNamedArg(args)) {
            val paramBoundsByName = paramNames.zip(pt.paramInfos).toMap

            // Type arguments which are specified by name (immutable after this first loop)
            val namedArgMap = new mutable.HashMap[Name, Type]
            for (NamedArg(name, arg) <- args)
              if (namedArgMap.contains(name))
                report.error(DuplicateNamedTypeParameter(name), arg.sourcePos)
              else if (!paramNames.contains(name))
                report.error(UndefinedNamedTypeParameter(name, paramNames), arg.sourcePos)
              else
                namedArgMap(name) = arg.tpe

            // Holds indexes of non-named typed arguments in paramNames
            val gapBuf = new mutable.ListBuffer[Int]
            def nextPoly(idx: Int) = {
              val newIndex = gapBuf.length
              gapBuf += idx
              // Re-index unassigned type arguments that remain after transformation
              pt.paramRefs(newIndex)
            }

            // Type parameters after naming assignment, conserving paramNames order
            val normArgs: List[Type] = paramNames.zipWithIndex.map { case (pname, idx) =>
              namedArgMap.getOrElse(pname, nextPoly(idx))
            }

            val transform = new TypeMap {
              def apply(t: Type) = t match {
                case TypeParamRef(`pt`, idx) => normArgs(idx)
                case _ => mapOver(t)
              }
            }
            val resultType1 = transform(pt.resultType)
            if (gapBuf.isEmpty) resultType1
            else {
              val gaps = gapBuf.toList
              pt.derivedLambdaType(
                gaps.map(paramNames),
                gaps.map(idx => transform(pt.paramInfos(idx)).bounds),
                resultType1)
            }
          }
          else {
            val argTypes = args.tpes
            if (sameLength(argTypes, paramNames)) pt.instantiate(argTypes)
            else wrongNumberOfTypeArgs(fn.tpe, pt.typeParams, args, tree.sourcePos)
          }
        }
      case err: ErrorType =>
        tree.withType(err)
      case ref: TermRef if ref.isOverloaded =>
        val disambiguated = ref.denot.suchThat(_.info.isInstanceOf[PolyType])
        if (disambiguated.exists) {
          val fn1 = fn.withType(ref.withDenot(disambiguated))
          val tree1 = untpd.cpy.TypeApply(tree)(fn1, args)
          assignType(tree1, fn1, args)
        }
        else fail
      case _ =>
        //println(i"bad type: $fn: ${fn.symbol} / ${fn.symbol.isType} / ${fn.symbol.info}") // DEBUG
        fail
    })
  }

  def assignType(tree: untpd.Typed, tpt: Tree)(using Ctx, CState): Typed =
    tree.withType(tpt.tpe)

  def assignType(tree: untpd.NamedArg, arg: Tree)(using Ctx, CState): NamedArg =
    tree.withType(arg.tpe)

  def assignType(tree: untpd.Assign)(using Ctx, CState): Assign =
    tree.withType(defn.UnitType)

  def assignType(tree: untpd.Block, stats: List[Tree], expr: Tree)(using Ctx, CState): Block =
    tree.withType(avoidingType(expr, stats))

  def assignType(tree: untpd.Inlined, bindings: List[Tree], expansion: Tree)(using Ctx, CState): Inlined =
    tree.withType(avoidingType(expansion, bindings))

  def assignType(tree: untpd.If, thenp: Tree, elsep: Tree)(using Ctx, CState): If =
    tree.withType(thenp.tpe | elsep.tpe)

  def assignType(tree: untpd.Closure, meth: Tree, target: Tree)(using Ctx, CState): Closure =
    tree.withType(
      if (target.isEmpty) meth.tpe.widen.toFunctionType(tree.env.length)
      else target.tpe)

  def assignType(tree: untpd.CaseDef, pat: Tree, body: Tree)(using Ctx, CState): CaseDef = {
    val ownType =
      if (body.isType) {
        val params = new TreeAccumulator[mutable.ListBuffer[TypeSymbol]] {
          def apply(ps: mutable.ListBuffer[TypeSymbol], t: Tree)(using Ctx, CState) = t match {
            case t: Bind if t.symbol.isType => foldOver(ps += t.symbol.asType, t)
            case _ => foldOver(ps, t)
          }
        }
        HKTypeLambda.fromParams(
          params(new mutable.ListBuffer[TypeSymbol](), pat).toList,
          defn.MatchCase(pat.tpe, body.tpe))
      }
      else body.tpe
    tree.withType(ownType)
  }

  def assignType(tree: untpd.Match, scrutinee: Tree, cases: List[CaseDef])(using Ctx, CState): Match =
    tree.withType(ctx.typeComparer.lub(cases.tpes))

  def assignType(tree: untpd.Labeled)(using Ctx, CState): Labeled =
    tree.withType(tree.bind.symbol.info)

  def assignType(tree: untpd.Return)(using Ctx, CState): Return =
    tree.withType(defn.NothingType)

  def assignType(tree: untpd.WhileDo)(using Ctx, CState): WhileDo =
    tree.withType(if (tree.cond eq EmptyTree) defn.NothingType else defn.UnitType)

  def assignType(tree: untpd.Try, expr: Tree, cases: List[CaseDef])(using Ctx, CState): Try =
    if (cases.isEmpty) tree.withType(expr.tpe)
    else tree.withType(ctx.typeComparer.lub(expr.tpe :: cases.tpes))

  def assignType(tree: untpd.SeqLiteral, elems: List[Tree], elemtpt: Tree)(using Ctx, CState): SeqLiteral = {
    val ownType = tree match {
      case tree: untpd.JavaSeqLiteral => defn.ArrayOf(elemtpt.tpe)
      case _ => if (currentlyAfterErasure) defn.SeqType else defn.SeqType.appliedTo(elemtpt.tpe)
    }
    tree.withType(ownType)
  }

  def assignType(tree: untpd.SingletonTypeTree, ref: Tree)(using Ctx, CState): SingletonTypeTree =
    tree.withType(ref.tpe)

  /** Assign type of RefinedType.
   *  Refinements are typed as if they were members of refinement class `refineCls`.
   */
  def assignType(tree: untpd.RefinedTypeTree, parent: Tree, refinements: List[Tree], refineCls: ClassSymbol)(using Ctx, CState): RefinedTypeTree = {
    def addRefinement(parent: Type, refinement: Tree): Type = {
      val rsym = refinement.symbol
      val rinfo = if (rsym.is(Accessor)) rsym.info.resultType else rsym.info
      if (rinfo.isError) rinfo
      else if (!rinfo.exists) parent // can happen after failure in self type definition
      else RefinedType(parent, rsym.name, rinfo)
    }
    val refined = refinements.foldLeft(parent.tpe)(addRefinement)
    tree.withType(RecType.closeOver(rt => refined.substThis(refineCls, rt.recThis)))
  }

  def assignType(tree: untpd.AppliedTypeTree, tycon: Tree, args: List[Tree])(using Ctx, CState): AppliedTypeTree = {
    assert(!hasNamedArg(args) || ctx.reporter.errorsReported, tree)
    val tparams = tycon.tpe.typeParams
    val ownType =
      if (sameLength(tparams, args))
        if (tycon.symbol == defn.andType) AndType(args(0).tpe, args(1).tpe)
        else if (tycon.symbol == defn.orType) OrType(args(0).tpe, args(1).tpe)
        else tycon.tpe.appliedTo(args.tpes)
      else wrongNumberOfTypeArgs(tycon.tpe, tparams, args, tree.sourcePos)
    tree.withType(ownType)
  }

  def assignType(tree: untpd.LambdaTypeTree, tparamDefs: List[TypeDef], body: Tree)(using Ctx, CState): LambdaTypeTree =
    tree.withType(HKTypeLambda.fromParams(tparamDefs.map(_.symbol.asType), body.tpe))

  def assignType(tree: untpd.MatchTypeTree, bound: Tree, scrutinee: Tree, cases: List[CaseDef])(using Ctx, CState): MatchTypeTree = {
    val boundType = if (bound.isEmpty) defn.AnyType else bound.tpe
    tree.withType(MatchType(boundType, scrutinee.tpe, cases.tpes))
  }

  def assignType(tree: untpd.ByNameTypeTree, result: Tree)(using Ctx, CState): ByNameTypeTree =
    tree.withType(ExprType(result.tpe))

  def assignType(tree: untpd.TypeBoundsTree, lo: Tree, hi: Tree, alias: Tree)(using Ctx, CState): TypeBoundsTree =
    tree.withType(
      if !alias.isEmpty then alias.tpe
      else if lo eq hi then TypeAlias(lo.tpe)
      else TypeBounds(lo.tpe, hi.tpe))

  def assignType(tree: untpd.Bind, sym: Symbol)(using Ctx, CState): Bind =
    tree.withType(NamedType(NoPrefix, sym))

  def assignType(tree: untpd.Alternative, trees: List[Tree])(using Ctx, CState): Alternative =
    tree.withType(ctx.typeComparer.lub(trees.tpes))

  def assignType(tree: untpd.UnApply, proto: Type)(using Ctx, CState): UnApply =
    tree.withType(proto)

  def assignType(tree: untpd.ValDef, sym: Symbol)(using Ctx, CState): ValDef =
    tree.withType(if (sym.exists) assertExists(sym.termRef) else NoType)

  def assignType(tree: untpd.DefDef, sym: Symbol)(using Ctx, CState): DefDef =
    tree.withType(sym.termRef)

  def assignType(tree: untpd.TypeDef, sym: Symbol)(using Ctx, CState): TypeDef =
    tree.withType(sym.typeRef)

  def assertExists(tp: Type): Type = { assert(tp != NoType); tp }

  def assignType(tree: untpd.Import, sym: Symbol)(using Ctx, CState): Import =
    tree.withType(sym.termRef)

  def assignType(tree: untpd.Annotated, arg: Tree, annot: Tree)(using Ctx, CState): Annotated = {
    assert(tree.isType) // annotating a term is done via a Typed node, can't use Annotate directly
    tree.withType(AnnotatedType(arg.tpe, Annotation(annot)))
  }

  def assignType(tree: untpd.PackageDef, pid: Tree)(using Ctx, CState): PackageDef =
    tree.withType(pid.symbol.termRef)
}


object TypeAssigner extends TypeAssigner

