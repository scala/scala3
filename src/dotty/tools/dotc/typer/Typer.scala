package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._
import Constants._
import StdNames._
import Scopes._
import Denotations._
import Inferencing._
import Contexts._
import Symbols._
import Types._
import SymDenotations._
import Names._
import NameOps._
import Flags._
import Decorators._
import ErrorReporting._
import Applications.{FunProtoType, PolyProtoType}
import EtaExpansion.etaExpand
import util.Positions._
import util.SourcePosition
import collection.mutable
import annotation.tailrec
import language.implicitConversions

trait TyperContextOps { ctx: Context => }

object Typer {

  import tpd._

  object BindingPrec {
    val definition = 4
    val namedImport = 3
    val wildImport = 2
    val packageClause = 1
    val nothingBound = 0
    def isImportPrec(prec: Int) = prec == namedImport || prec == wildImport
  }

  implicit class TreeDecorator(tree: Tree) {
    def exprType(implicit ctx: Context): Type = tree.tpe match {
      case tpe: TermRef if !tpe.symbol.isStable => tpe.info
      case tpe => tpe
    }
  }

  case class StateFul[T](value: T, state: TyperState) {
    def commit()(implicit ctx: Context): T = {
      state.commit()
      value
    }
  }
}

class Typer extends Namer with Applications with Implicits {

  import tpd._
  import Typer._

  /** A temporary data item valid for a single typed ident:
   *  The set of all root import symbols that have been
   *  encountered as a qualifier of an import so far.
   */
  private var importedFromRoot: Set[Symbol] = Set()

  def typedSelection(site: Type, name: Name, pos: Position)(implicit ctx: Context): Type = {
    val ref =
      if (name == nme.CONSTRUCTOR) site.decl(name)
      else site.member(name)
    if (ref.exists) NamedType(site, name).withDenot(ref)
    else {
      if (!site.isErroneous)
        ctx.error(
          if (name == nme.CONSTRUCTOR) s"${site.show} does not have a constructor"
          else s"$name is not a member of ${site.show}", pos)
      ErrorType
    }
  }

  def checkAccessible(tpe: Type, superAccess: Boolean, pos: Position)(implicit ctx: Context): Type = tpe match {
    case tpe: NamedType =>
      val pre = tpe.prefix
      val name = tpe.name
      val d = tpe.denot.accessibleFrom(pre, superAccess)
      if (!d.exists) {
        val alts = tpe.denot.alternatives.map(_.symbol).filter(_.exists)
        val where = pre.typeSymbol
        val what = alts match {
          case Nil =>
            name.toString
          case sym :: Nil =>
            if (sym.owner == where) sym.show else sym.showLocated
          case _ =>
            s"none of the overloaded alternatives named $name"
        }
        val whyNot = new StringBuffer
        val addendum =
          alts foreach (_.isAccessibleFrom(pre, superAccess, whyNot))
        ctx.error(s"$what cannot be accessed in $where.$whyNot")
        ErrorType
      } else tpe withDenot d
    case _ =>
      tpe
  }

  /** The qualifying class
   *  of a this or super with prefix `qual`.
   *  packageOk is equal false when qualifying class symbol
   */
  def qualifyingClass(tree: untpd.Tree, qual: Name, packageOK: Boolean)(implicit ctx: Context): Symbol =
    ctx.owner.enclosingClass.ownersIterator.find(o => qual.isEmpty || o.isClass && o.name == qual) match {
      case Some(c) if packageOK || !(c is Package) =>
        c
      case _ =>
        ctx.error(
          if (qual.isEmpty) tree.show + " can be used only in a class, object, or template"
          else qual.show + " is not an enclosing class", tree.pos)
        NoSymbol
    }

  /** Attribute an identifier consisting of a simple name or an outer reference.
   *
   *  @param tree      The tree representing the identifier.
   *  Transformations: (1) Prefix class members with this.
   *                   (2) Change imported symbols to selections
   *
   */
  def typedIdent(tree: untpd.Ident)(implicit ctx: Context): Tree = {
    val name = tree.name

    /** Is this import a root import that has been shadowed by an explicit
     *  import in the same program?
     */
    def isDisabled(imp: ImportInfo, site: Type): Boolean = {
      val qualSym = site.termSymbol
      if (defn.RootImports contains qualSym) {
        if (imp.rootImport && (importedFromRoot contains qualSym)) return true
        importedFromRoot += qualSym
      }
      false
    }

    /** Does this identifier appear as a constructor of a pattern? */
    def isPatternConstr =
      if (ctx.mode.isExpr && (ctx.outer.mode is Mode.Pattern))
        ctx.outer.tree match {
          case Apply(`tree`, _) => true
          case _ => false
        }
      else false

    /** A symbol qualifies if it exists and is not stale. Stale symbols
     *  are made to disappear here. In addition,
     *  if we are in a constructor of a pattern, we ignore all definitions
     *  which are methods (note: if we don't do that
     *  case x :: xs in class List would return the :: method)
     *  unless they are stable or are accessors (the latter exception is for better error messages)
     */
    def qualifies(sym: Symbol): Boolean = !(
         sym.isAbsent
      || isPatternConstr && (sym is (Method, butNot = Accessor))
      )

    /** Find the denotation of enclosing `name` in given context `ctx`.
     *  @param previous    A denotation that was found in a more deeply nested scope,
     *                     or else `NoDenotation` if nothing was found yet.
     *  @param prevPrec    The binding precedence of the previous denotation,
     *                     or else `nothingBound` if nothing was found yet.
     *  @param prevCtx     The context of the previous denotation,
     *                     or else `NoContext` if nothing was found yet.
     */
    def findRef(previous: Type, prevPrec: Int, prevCtx: Context)(implicit ctx: Context): Type = {
      import BindingPrec._

      /** A string which explains how something was bound; Depending on `prec` this is either
       *      imported by <tree>
       *  or  defined in <symbol>
       */
      def bindingString(prec: Int, whereFound: Context, qualifier: String = "") =
        if (prec == wildImport || prec == namedImport) s"imported$qualifier by  ${whereFound.tree.show}"
        else s"defined$qualifier in ${whereFound.owner.show}"

      /** Check that any previously found result from an inner context
       *  does properly shadow the new one from an outer context.
       */
      def checkNewOrShadowed(found: Type, newPrec: Int): Type =
        if (!previous.exists || (previous == found)) found
        else {
          if (!previous.isError && !found.isError)
            ctx.error(
              s"""reference to $name is ambiguous;
                 |it is both ${bindingString(newPrec, ctx, "")}
                 |and ${bindingString(prevPrec, prevCtx, " subsequently")}""".stripMargin,
              tree.pos)
          previous
        }

      /** The type representing a named import with enclosing name when imported
       *  from given `site` and `selectors`.
       */
      def namedImportRef(site: Type, selectors: List[untpd.Tree]): Type = {
        def checkUnambiguous(found: Type) = {
          val other = namedImportRef(site, selectors.tail)
          if (other.exists && (found != other))
            ctx.error(s"""reference to $name is ambiguous; it is imported twice in
                         |${ctx.tree.show}""".stripMargin,
                      tree.pos)
          found
        }
        selectors match {
          case Pair(Ident(from), Ident(`name`)) :: rest =>
            checkUnambiguous(typedSelection(site, name, tree.pos))
          case Ident(`name`) :: rest =>
            checkUnambiguous(typedSelection(site, name, tree.pos))
          case _ :: rest =>
            namedImportRef(site, rest)
          case nil =>
            NoType
        }
      }

      /** The type representing a wildcard import with enclosing name when imported
       *  from given import info
       */
      def wildImportRef(imp: ImportInfo): Type = {
        if (imp.wildcardImport && !(imp.excluded contains name.toTermName)) {
          val pre = imp.site
          if (!isDisabled(imp, pre)) {
            val denot = pre.member(name)
            if (denot.exists) return NamedType(pre, name).withDenot(denot)
          }
        }
        NoType
      }

      /** Is (some alternative of) the given predenotation `denot`
       *  defined in current compilation unit?
       */
      def isDefinedInCurrentUnit(denot: PreDenotation): Boolean = denot match {
        case DenotUnion(d1, d2) => isDefinedInCurrentUnit(d1) || isDefinedInCurrentUnit(d2)
        case denot: SingleDenotation => denot.symbol.sourceFile == ctx.source
      }

      // begin findRef
      if (ctx eq NoContext) previous
      else {
        val outer = ctx.outer
        if (ctx.scope ne outer.scope) {
          val defDenots = ctx.lookup(name)
          if (defDenots.exists) {
            val curOwner = ctx.owner
            val pre = curOwner.thisType
            val found = NamedType(pre, name).withDenot(defDenots toDenot pre)
            if (!(curOwner is Package) || isDefinedInCurrentUnit(defDenots))
              return checkNewOrShadowed(found, definition) // no need to go further out, we found highest prec entry
            else if (prevPrec < packageClause)
              return findRef(found, packageClause, ctx)(outer)
          }
        }
        val curImport = ctx.importInfo
        if (prevPrec < namedImport && (curImport ne outer.importInfo)) {
          val namedImp = namedImportRef(curImport.site, curImport.selectors)
          if (namedImp.exists)
            return findRef(checkNewOrShadowed(namedImp, namedImport), namedImport, ctx)(outer)
          if (prevPrec < wildImport) {
            val wildImp = wildImportRef(curImport)
            if (wildImp.exists)
              return findRef(checkNewOrShadowed(wildImp, wildImport), wildImport, ctx)(outer)
          }
        }
        findRef(previous, prevPrec, prevCtx)(outer)
      }
    }

    // begin typedIdent
    val startingContext = // ignore current variable scope in patterns to enforce linearity
      if (ctx.mode is Mode.Pattern) ctx.outer else ctx
    val saved = importedFromRoot
    importedFromRoot = Set()

    val rawType =
      try findRef(NoType, BindingPrec.nothingBound, NoContext)
      finally importedFromRoot = saved

    val ownType =
      if (rawType.exists) checkAccessible(rawType, superAccess = false, tree.pos)
      else {
        ctx.error(s"not found: $name", tree.pos)
        ErrorType
      }
    tree.withType(ownType.underlyingIfRepeated)
  }

  def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
    val qual1 = typedExpr(tree.qualifier, RefinedType(WildcardType, tree.name, pt))
    val ownType = typedSelection(qual1.exprType, tree.name, tree.pos)
    if (!ownType.isError) checkAccessible(ownType, qual1.isInstanceOf[Super], tree.pos)
    tree.withType(ownType).derivedSelect(qual1, tree.name)
  }

  def typedThis(tree: untpd.This)(implicit ctx: Context): Tree = {
    val cls = qualifyingClass(tree, tree.qual, packageOK = false)
    tree.withType(cls.thisType)
  }

  def typedSuper(tree: untpd.Super)(implicit ctx: Context): Tree = {
    val mix = tree.mix
    val qual1 = typed(tree.qual)
    val cls = qual1.tpe.typeSymbol

    def findMixinSuper(site: Type): Type = site.parents filter (_.name == mix) match {
      case p :: Nil =>
        p
      case Nil =>
        errorType(s"$mix does not name a parent class of $cls", tree.pos)
      case p :: q :: _ =>
        errorType(s"ambiguous parent class qualifier", tree.pos)
    }
    val owntype =
      if (!mix.isEmpty) findMixinSuper(cls.info)
      else if (ctx.mode is Mode.InSuperInit) cls.info.firstParent
      else cls.info.parents.reduceLeft((x: Type, y: Type) => AndType(x, y))

    tree.withType(SuperType(cls.thisType, owntype)).derivedSuper(qual1, mix)
  }

  def typedLiteral(tree: untpd.Literal)(implicit ctx: Context) =
    tree.withType(if (tree.const.tag == UnitTag) defn.UnitType else ConstantType(tree.const))

  def typedNew(tree: untpd.New)(implicit ctx: Context) = {
    val tpt1 = typedType(tree.tpt)
    val cls = checkClassTypeWithStablePrefix(tpt1.tpe, tpt1.pos)
    checkInstantiatable(cls, tpt1.pos)
    tree.withType(tpt1.tpe).derivedNew(tpt1)
  }

  def typedPair(tree: untpd.Pair)(implicit ctx: Context) = {
    val left1 = typed(tree.left)
    val right1 = typed(tree.right)
    tree.withType(defn.PairType.appliedTo(left1.tpe :: right1.tpe :: Nil)).derivedPair(left1, right1)
  }

  def TypedTyped(tree: untpd.Typed)(implicit ctx: Context) = {
    val tpt1 = typedType(tree.tpt)
    val expr1 = typedExpr(tree.expr, tpt1.tpe)
    tree.withType(tpt1.tpe).derivedTyped(tpt1, expr1)
  }

  def NamedArg(tree: untpd.NamedArg, pt: Type)(implicit ctx: Context) = {
    val arg1 = typed(tree.arg, pt)
    tree.withType(arg1.tpe).derivedNamedArg(tree.name, arg1)
  }

  def Assign(tree: untpd.Assign)(implicit ctx: Context) = {
    ???
  }

  def typedModifiers(mods: untpd.Modifiers)(implicit ctx: Context): Modifiers = {
    val annotations1 = mods.annotations mapconserve typedAnnotation
    if (annotations1 eq mods.annotations) mods.asInstanceOf[Modifiers]
    else Modifiers(mods.flags, mods.privateWithin, annotations1)
  }

  def typedAnnotation(annot: untpd.Tree)(implicit ctx: Context): Tree =
    typed(annot, defn.AnnotationClass.typeConstructor)

  def typedValDef(vdef: untpd.ValDef, sym: Symbol)(implicit ctx: Context) = {
    val ValDef(mods, name, tpt, rhs) = vdef
    val mods1 = typedModifiers(mods)
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    val pt = if (sym.exists) sym.symRef else NoType
    vdef.withType(pt).derivedValDef(mods1, name, tpt1, rhs1)
  }

  def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
    val DefDef(mods, name, tparams, vparamss, tpt, rhs) = ddef
    val mods1 = typedModifiers(mods)
    val tparams1 = tparams mapconserve (typed(_).asInstanceOf[TypeDef])
    val vparamss1 = vparamss.mapconserve(_ mapconserve (typed(_).asInstanceOf[ValDef]))
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    ddef.withType(sym.symRef).derivedDefDef(mods1, name, tparams1, vparamss1, tpt1, rhs1)
    //todo: make sure dependent method types do not depend on implicits or by-name params
  }

  def typedTypeDef(tdef: untpd.TypeDef, sym: Symbol)(implicit ctx: Context): TypeDef = {
    val TypeDef(mods, name, rhs) = tdef
    val mods1 = typedModifiers(mods)
    val rhs1 = typedType(rhs)
    tdef.withType(sym.symRef).derivedTypeDef(mods1, name, rhs1)
  }

  def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(implicit ctx: Context) = {
    val TypeDef(mods, name, impl @ Template(constr, parents, self, body)) = cdef
    val mods1 = typedModifiers(mods)
    val constr1 = typed(constr).asInstanceOf[DefDef]
    val parents1 = parents mapconserve (typed(_))
    val self1 = self.withType(NoType).derivedValDef(
      typedModifiers(self.mods), self.name, typedType(self.tpt), EmptyTree)

    val localDummy = ctx.newLocalDummy(cls, impl.pos)
    val body1 = typedStats(body, localDummy)(inClassContext(cls, self.name))
    val impl1 = impl.withType(localDummy.symRef).derivedTemplate(
      constr1, parents1, self1, body1)

    cdef.withType(cls.symRef).derivedTypeDef(mods1, name, impl1)

    // todo later: check that
    //  1. If class is non-abstract, it is instantiatable:
    //  - self type is s supertype of own type
    //  - all type members have consistent bounds
    // 2. all private type members have consistent bounds
    // 3. Types do not override classes.
    // 4. Polymorphic type defs override nothing.
  }

  def typedImport(imp: untpd.Import, sym: Symbol)(implicit ctx: Context): Import = {
    val expr1 = typedExpr(imp.expr)
    imp.withType(sym.symRef).derivedImport(expr1, imp.selectors)
  }

  def typedExpanded(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
    val sym = symOfTree.remove(tree).getOrElse(NoSymbol)
    sym.ensureCompleted()
    def localContext = ctx.fresh.withOwner(sym)
    typedTree remove tree match {
      case Some(tree1) => tree1
      case none => tree match {
        case tree: untpd.ValDef =>
          typedValDef(tree, sym)(localContext)
        case tree: untpd.DefDef =>
          val typer1 = nestedTyper.remove(sym).get
          typer1.typedDefDef(tree, sym)(localContext.withTyper(typer1))
        case tree: untpd.TypeDef =>
          if (tree.isClassDef) typedClassDef(tree, sym.asClass)(localContext)
          else typedTypeDef(tree, sym)(localContext.withNewScope)
        case tree: untpd.Import =>
          typedImport(tree, sym)
        case tree: untpd.TypeTree =>
          if (!tree.isEmpty) typedType(tree.original, pt)
          else {
            assert(!pt.isInstanceOf[WildcardType])
            tree.withType(pt)
          }
        case untpd.EmptyTree =>
          tpd.EmptyTree
      }
    }
  }

  def typed(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
    val xtree =
      tree match {
        case tree: untpd.MemberDef =>
          expandedTree remove tree match {
            case Some(xtree) => xtree
            case none => tree
          }
        case _ => tree
    }
    typedExpanded(xtree, pt)
  }

  def typedTrees(trees: List[untpd.Tree])(implicit ctx: Context): List[Tree] =
    trees mapconserve (typed(_))

  def typedStats(stats: List[untpd.Tree], exprOwner: Symbol)(implicit ctx: Context): List[tpd.Tree] = {
    val buf = new mutable.ListBuffer[Tree]
    @tailrec def traverse(stats: List[untpd.Tree])(implicit ctx: Context): List[Tree] = stats match {
      case (imp: untpd.Import) :: rest =>
        val imp1 = typed(imp)
        buf += imp1
        traverse(rest)(importContext(imp1.symbol, imp.selectors))
      case (mdef: untpd.MemberDef) :: rest =>
        buf += typed(mdef)
        traverse(rest)
      case stat :: rest =>
        buf += typed(stat)(ctx.fresh.withOwner(exprOwner))
        traverse(rest)
      case _ =>
        buf.toList
    }
    traverse(stats)
  }

  def typedExpr(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, pt)(ctx retractMode Mode.PatternOrType)
  def typedType(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, pt)(ctx addMode Mode.Type)
  def typedPattern(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, pt)(ctx addMode Mode.Pattern)

  def tryEither[T](op: Context => T)(fallBack: StateFul[T] => T)(implicit ctx: Context) = {
    val nestedCtx = ctx.fresh.withNewTyperState
    val result = op(nestedCtx)
    if (nestedCtx.reporter.hasErrors)
      fallBack(StateFul(result, nestedCtx.typerState))
    else {
      nestedCtx.typerState.commit()
      result
    }
  }

  def tryInsertApply(tree: Tree, pt: Type)(fallBack: StateFul[Tree] => Tree)(implicit ctx: Context): Tree =
    tryEither {
      implicit ctx => typedSelect(Trees.Select(untpd.TypedSplice(tree), nme.apply), pt)
    } {
      fallBack
    }

    /**
     *  (-1) For expressions with annotated types, let AnnotationCheckers decide what to do
     *  (0) Convert expressions with constant types to literals (unless in interactive/scaladoc mode)
     */

    /** Perform the following adaptations of expression, pattern or type `tree` wrt to
     *  given prototype `pt`:
     *  (1) Resolve overloading
     *  (2) Apply parameterless functions
     *  (3) Apply polymorphic types to fresh instances of their type parameters and
     *      store these instances in context.undetparams,
     *      unless followed by explicit type application.
     *  (4) Do the following to unapplied methods used as values:
     *  (4.1) If the method has only implicit parameters pass implicit arguments
     *  (4.2) otherwise, if `pt` is a function type and method is not a constructor,
     *        convert to function by eta-expansion,
     *  (4.3) otherwise, if the method is nullary with a result type compatible to `pt`
     *        and it is not a constructor, apply it to ()
     *  otherwise issue an error
     *  (5) Convert constructors in a pattern as follows:
     *  (5.1) If constructor refers to a case class factory, set tree's type to the unique
     *        instance of its primary constructor that is a subtype of the expected type.
     *  (5.2) If constructor refers to an extractor, convert to application of
     *        unapply or unapplySeq method.
     *
     *  (6) Convert all other types to TypeTree nodes.
     *  (7) When in TYPEmode but not FUNmode or HKmode, check that types are fully parameterized
     *      (7.1) In HKmode, higher-kinded types are allowed, but they must have the expected kind-arity
     *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
     *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
     *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
     *  (10) If the expected type is Byte, Short or Char, and the expression
     *      is an integer fitting in the range of that type, convert it to that type.
     *  (11) Widen numeric literals to their expected type, if necessary
     *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is scala.Unit.
     *  (13) When in mode EXPRmode, apply AnnotationChecker conversion if expected type is annotated.
     *  (14) When in mode EXPRmode, apply a view
     *  If all this fails, error
     */
  def adapt(tree: Tree, pt: Type)(implicit ctx: Context): Tree = {

    def adaptOverloaded(ref: TermRef) = {
      val altDenots = ref.denot.alternatives
      val alts = altDenots map (alt =>
        TermRef.withSym(ref.prefix, alt.symbol.asTerm))
      def expectedStr = err.expectedTypeStr(pt)
      resolveOverloaded(alts, pt) match {
        case alt :: Nil =>
          adapt(tree.withType(alt), pt)
        case Nil =>
          def noMatches =
            errorTree(tree,
              s"""none of the ${err.overloadedAltsStr(altDenots)}
                 |match $expectedStr""".stripMargin)
          pt match {
            case pt: FunProtoType => tryInsertApply(tree, pt)(_ => noMatches)
            case _ => noMatches
          }
        case alts =>
          errorTree(tree,
            s"""Ambiguous overload. The ${err.overloadedAltsStr(altDenots take 2)}
               |both match $expectedStr""".stripMargin)
      }
    }

    def adaptToArgs(tp: Type, pt: FunProtoType) = tp match {
      case _: MethodType => tree
      case _ => tryInsertApply(tree, pt) {
        def fn = err.refStr(TreeInfo.methPart(tree).tpe)
        val more = tree match {
          case Apply(_, _) => " more"
          case _ => ""
        }
        _ => errorTree(tree, s"$fn does not take$more parameters")
      }
    }

    def adaptNoArgs(tp: Type) = tp match {
      case tp: ExprType =>
        adapt(tree.withType(tp.resultType), pt)
      case tp: ImplicitMethodType =>
        val args = tp.paramTypes map (inferImplicit(_, EmptyTree, tree.pos))
        adapt(tpd.Apply(tree, args), pt)
      case tp: MethodType =>
        if (defn.isFunctionType(pt) && !tree.symbol.isConstructor)
          etaExpand(tree, tp)
        else if (tp.paramTypes.isEmpty)
          adapt(tpd.Apply(tree, Nil), pt)
        else
          errorTree(tree,
            s"""missing arguments for ${tree.symbol.show}
               |follow this method with `_' if you want to treat it as a partially applied function""".stripMargin)
      case _ =>
        if (tp <:< pt) tree else adaptToSubType(tp)
    }

    def adaptToSubType(tp: Type): Tree = {
      val adapted = ConstFold(tree, pt)
      if (adapted ne EmptyTree) return adapted
      if (ctx.mode.isExpr) {
        if (pt.typeSymbol == defn.UnitClass)
          return tpd.Block(tree :: Nil, Literal(Constant()))
        val adapted = inferView(tree, pt)
        if (adapted ne EmptyTree) return adapted
      }
      err.typeMismatch(tree, pt)
    }

    tree.tpe.widen match {
      case ref: TermRef =>
        adaptOverloaded(ref)
      case poly: PolyType =>
        if (pt.isInstanceOf[PolyProtoType]) tree
        else {
          val tracked = ctx.track(poly)
          val tvars = ctx.newTypeVars(tracked)
          adapt(tpd.TypeApply(tree, tvars map (tpd.TypeTree(_))), pt)
        }
      case tp =>
        pt match {
          case pt: FunProtoType => adaptToArgs(tp, pt)
          case _ => adaptNoArgs(tp)
        }
    }
  }
}