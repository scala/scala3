package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._, Constants._, StdNames._, Scopes._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import ast.desugar, ast.desugar._
import util.Positions._
import util.SourcePosition
import collection.mutable
import language.implicitConversions

trait NamerContextOps { this: Context =>

  def enter(sym: Symbol): Symbol = {
    ctx.owner match {
      case cls: ClassSymbol => cls.enter(sym)
      case _ => this.scope.asInstanceOf[MutableScope].enter(sym)
    }
    sym
  }
}

abstract class Namer { typer: Typer =>

  import untpd._

  /** There are three maps at play here:
   *
   *
   *  Original tree ------------> Expanded tree(s)
   *                expandedTree     : ^
   *                  (weak)         : |
   *                                 : |
   *                symOfUntypedTree : | untypedTreeOfSym
   *                                 : |
   *                                 v |  typedTreeOfSym
   *                               Symbol --------------> Typed tree
   *                                      <::::::::::::::
   *                                      symOfTypedTree
   *
   *  The expandedTree map is weak, the others are strong.
   *
   *  The untypedTreeOfSym map can be inverted to a map from untyped trees
   *  to the symbols they define. The function `symOfUntypedTree` looks up a
   *  symbol in the current context with the name of the tree and which points
   *  back (via untypedTreeOfSym) to the tree. Similarly, `typedTreeOfSym` can be
   *  inverted to `symofTypedTree`.
   *
   *  Timeline:
   *
   *  During enter, trees are expanded as necessary, populating the expandedTree map.
   *  Symbols are created, and the untypedTreeOfSym link is set up.
   *
   *  Symbol completion causes some trees to be already typechecked and typedTreeOfSym
   *  entries are created to associate the typed trees with the untyped expanded originals.
   *
   *  During typer, original trees are first expanded using expandedTree. For each
   *  expanded definition tree, we make sure the corresponding symbol is completed
   *  and remove its untypedTreeOfSym link. We then consult the typedTreeOfSym map.
   *  If a typed tree exists, it replaces the original untyped tree, and the corresponding
   *  entry in the typedTree map is removed. Otherwise the untyped tree is typechecked,
   *  yielding the typed tree.
   *
   *  Dealing with shared tree nodes:
   *
   *  The scheme is designed to allow arbitrary sharing of nodes: (1) The expansion of
   *  a tree is context free, so expanding trees several times yields the same result
   *  as expanding once. No need to lock or duplicate expandedTree items. (2)
   *  Each `enterSyms` pass over a shared node creates new symbols and the two remaining
   *  maps are indexed with these symbols, so no sharing occurs for them.
   *
   *  Memory reclamation:
   *
   *  expandedTrees is a weak map, so entries will be reclaimed once the original
   *  untyped tree is no longer referenced. typedTreeOfSym and untypedTreeOfSym
   *  entries are both removed by the time a definition is integrated in the typed tree
   *  during phase typer.
   */

  val expandedTree = new mutable.WeakHashMap[MemberDef, Tree]

  val untypedTreeOfSym = mutable.Map[Symbol, Tree]()

  val typedTreeOfSym = new mutable.HashMap[Symbol, tpd.Tree]

  implicit def posToCoord(pos: Position): Coord = positionCoord(pos)

  def enclosingStats(implicit ctx: Context): List[Trees.Tree[_ >: Untyped]] =
    if (ctx == NoContext) Nil
    else ctx.tree match {
      case Template(_, _, _, stats) => stats
      case Block(stats, _) => stats
      case PackageDef(_, stats) => stats
      case _ => enclosingStats(ctx.outer)
    }

  def privateWithinClass(mods: Modifiers)(implicit ctx: Context): Symbol = {
    val pw = mods.privateWithin
    if (pw.isEmpty) NoSymbol
    else {
      val cls = ctx.owner.enclosingClassNamed(pw)
      if (!cls.exists) ctx.error(s"no enclosing class or object is named $pw", mods.pos)
      cls
    }
  }

  def symOfTree(tree: Trees.NameTree[_], treeMap: collection.Map[Symbol, Trees.Tree[_]])(implicit ctx: Context): Symbol = {
    var e = ctx.scope.lookupEntry(tree.name)
    while (e != null && treeMap(e.sym) != tree)
      e = ctx.scope.lookupNextEntry(e)
    if (e == null) NoSymbol else e.sym
  }

  def symOfTypedTree(tree: tpd.NameTree)(implicit ctx: Context) = symOfTree(tree, typedTreeOfSym)(ctx)
  def symOfUntypedTree (tree: NameTree)(implicit ctx: Context) = symOfTree(tree, untypedTreeOfSym)(ctx)

  def createSymbol(tree: Tree)(implicit ctx: Context): Symbol = {
    val sym = tree match {
      case tree: ClassDef =>
        ctx.enter(ctx.newClassSymbol(
          ctx.owner, tree.name, tree.mods.flags, new Completer,
          privateWithinClass(tree.mods), tree.pos, ctx.source.file))
      case tree: MemberDef =>
        ctx.enter(ctx.newSymbol(
          ctx.owner, tree.name, tree.mods.flags, new Completer,
          privateWithinClass(tree.mods), tree.pos))
      case imp: Import =>
        ctx.newSymbol(
          ctx.owner, nme.IMPORT, Synthetic, new Completer, NoSymbol, tree.pos)
      case _ =>
        NoSymbol
    }
    if (sym.exists) untypedTreeOfSym(sym) = tree
    sym
  }

  def expansion(defn: MemberDef)(implicit ctx: Context): Tree = {
    val expanded = desugar.memberDef(defn)
    if (expanded ne defn) expandedTree(defn) = expanded
    expanded
  }

  def enterSym(stat: Tree)(implicit ctx: Context): Context = stat match {
    case imp: Import =>
      val sym = createSymbol(imp)
      ctx.fresh.withImport(ImportInfo(sym, imp.selectors, ctx.scopeNestingLevel))
    case defn: MemberDef =>
      expansion(defn).toList foreach createSymbol
      ctx
    case _ =>
      ctx
  }

  def enterSyms(stats: List[Tree])(implicit ctx: Context): Context = {

    def traverse(stats: List[Tree])(implicit ctx: Context): Context = stats match {
      case stat :: stats1 =>
        traverse(stats)(enterSym(stat))
      case nil =>
        ctx
    }

    def mergeCompanionDefs() = {
      val caseClassDef = mutable.Map[TypeName, ClassDef]()
      for (cdef @ ClassDef(mods, name, _) <- stats)
        if (mods is Case) caseClassDef(name) = cdef
      for (mdef @ ModuleDef(_, name, _) <- stats)
        caseClassDef get name.toTypeName match {
          case Some(cdef) =>
            val Thicket((mcls @ ClassDef(_, _, impl)) :: mrest) = expandedTree(mdef)
            val Thicket(cls :: (companion: ClassDef) :: crest) = expandedTree(cdef)
            val mcls1 = mcls.derivedClassDef(mcls.mods, mcls.name,
              impl.derivedTemplate(impl.constr, impl.parents, impl.self,
                companion.impl.body ++ impl.body))
            expandedTree(mdef) = Thicket(mcls1 :: mrest)
            expandedTree(cdef) = Thicket(cls :: crest)
          case none =>
        }
      }

    val result = traverse(stats)
    mergeCompanionDefs()
    result
  }

  def enterParams(ddef: DefDef)(ctx: Context): Context =
    (enterSyms(ddef.tparams)(ctx) /: ddef.vparamss) ((ctx, params) => enterSyms(params)(ctx))

  class Completer(implicit ctx: Context) extends LazyType {

    def complete(denot: SymDenotation): Unit = {
      val symToComplete = denot.symbol
      val original = untypedTreeOfSym(symToComplete)

      def inheritedResultType(paramFn: Type => Type)(implicit ctx: Context): Type = {
        lazy val schema = paramFn(WildcardType)
        val site = symToComplete.owner.symTypeRef
        ((NoType: Type) /: symToComplete.owner.info.baseClasses.tail) { (tp, cls) =>
          val itpe = cls.info
            .nonPrivateDecl(symToComplete.name)
            .matchingDenotation(site, schema)
            .asSeenFrom(site)
            .info.finalResultType
          tp & itpe
        }
      }

      def typedDefn(tree: Tree, sym: Symbol)(implicit ctx: Context): tpd.Tree = {
        val tree1 = typer.typed(tree, sym.symRef)
        typedTreeOfSym(sym) = tree1
        tree1
      }

      def valOrDefDefSig[UT <: untpd.ValOrDefDef, T <: tpd.ValOrDefDef]
              (defn: UT, op: DefTyper[UT, T], paramFn: Type => Type)(implicit ctx: Context): Type =
        paramFn {
          if (!defn.tpt.isEmpty) typer.typed(defn.tpt).tpe
          else {
            val inherited = inheritedResultType(paramFn)
            if (inherited.exists) typer.typed(defn.tpt, inherited).tpe
            else aheadDef(defn, op).tpt.tpe
          }
        }

      def completeParams[UT <: untpd.NameTree, T <: tpd.Tree]
        (params: List[UT], completer: DefTyper[UT, T])(implicit ctx: Context): Unit = {
        enterSyms(params)
        for (param <- params) aheadDef(param, completer)
      }

      def defDefSig(defn: DefDef)(implicit ctx: Context) = {
        val DefDef(_, _, tparams, vparamss, _, _) = defn
          completeParams(tparams, completeTypeDef)
          for (vparams <- vparamss) completeParams(vparams, completeValDef)
          def wrapMethType(restpe: Type): Type = {
            val monotpe =
              (restpe /: vparamss) { (restpe, params) =>
                val creator =
                  if (params.nonEmpty && (params.head.mods is Implicit)) ImplicitMethodType else MethodType
                creator.fromSymbols(params map symOfUntypedTree, restpe)
              }
            if (tparams.nonEmpty) PolyType.fromSymbols(tparams map symOfUntypedTree, monotpe)
            else if (vparamss.isEmpty) ExprType(monotpe)
            else monotpe
          }
          valOrDefDefSig(defn, completeDefDef, wrapMethType)
      }

      def typeDefSig(defn: TypeDef)(implicit ctx: Context): Type = {
        val lctx = localContext
        completeParams(defn.tparams, completeTypeDef)(lctx)
        val TypeDef(_, _, _, rhs) = aheadDef(defn, completeTypeDef)(lctx)
        rhs.tpe   // !!! do something about parameters!
      }

      def classDefSig(defn: ClassDef)(implicit ctx: Context): Type = {

        def parentType(constr: untpd.Tree): Type = {
          val Trees.Select(Trees.New(tpt), _) = TreeInfo.methPart(constr)
          val ptype = typedType(tpt).tpe
          if (ptype.uninstantiatedTypeParams.isEmpty) ptype else typedExpr(constr).tpe
        }

        def enterSelfSym(name: TermName, tpe: Type): Unit =
          ctx.enter(ctx.newSymbol(ctx.owner, name, Synthetic, tpe, coord = symToComplete.coord))

        val ClassDef(_, _, impl @ Template(constr, parents, self, body)) = defn

        val decls = newScope
        val (params, rest) = body span {
          case td: TypeDef => td.mods is ParamOrAccessor
          case _ => false
        }
        enterSyms(params)
        val parentTypes = parents map parentType
        val parentRefs = ctx.normalizeToRefs(parentTypes, symToComplete.asClass, decls)
        val selfTypeOpt = if (self.tpt.isEmpty) NoType else typedType(self.tpt).tpe
        if (self.name != nme.WILDCARD)
          enterSelfSym(self.name, selfTypeOpt orElse symToComplete.typeConstructor)
        enterSyms(rest)
        ClassInfo(denot.owner.thisType, symToComplete.asClass, parentRefs, decls, selfTypeOpt)
      }

      def localContext = ctx.fresh.withOwner(symToComplete)

      def typeSig(defn: Tree): Type = defn match {
        case defn: ValDef =>
          valOrDefDefSig(defn, completeValDef, identity)(localContext)
        case defn: DefDef =>
          defDefSig(defn)(localContext.withNewScope)
        case defn: TypeDef =>
          typeDefSig(defn)(localContext.withNewScope)
        case defn: ClassDef =>
          classDefSig(defn)(localContext)
        case imp: Import =>
          val expr1 = typedDefn(imp.expr, symToComplete)
          ImportType(SharedTree(expr1))
      }

      symToComplete.info = typeSig(original)
    }
  }
}