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

class Environment {
  import untpd._
  val scope = newScope
  val expandedTree = new mutable.WeakHashMap[MemberDef, Tree]
  val treeOfSym = mutable.Map[Symbol, Tree]()
  val typedTree = mutable.Map[Tree, tpd.Tree]()
}

class Namer { typer: Typer =>

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

  lazy val expandedTree = new mutable.WeakHashMap[Tree, Tree]
  lazy val symOfTree = new mutable.WeakHashMap[Tree, Symbol]
  lazy val typedTree = new mutable.WeakHashMap[Tree, tpd.Tree]
  lazy val nestedTyper = new mutable.HashMap[Symbol, Typer]

  val scope = newScope

  implicit def posToCoord(pos: Position): Coord = positionCoord(pos)

  def privateWithinClass(mods: Modifiers)(implicit ctx: Context): Symbol = {
    val pw = mods.privateWithin
    if (pw.isEmpty) NoSymbol
    else {
      val cls = ctx.owner.enclosingClassNamed(pw)
      if (!cls.exists) ctx.error(s"no enclosing class or object is named $pw", mods.pos)
      cls
    }
  }

  def createSymbol(tree: Tree)(implicit ctx: Context): Symbol = {
    val sym = tree match {
      case tree: ClassDef =>
        ctx.enter(ctx.newClassSymbol(
          ctx.owner, tree.name, tree.mods.flags, new Completer(tree),
          privateWithinClass(tree.mods), tree.pos, ctx.source.file))
      case tree: MemberDef =>
        ctx.enter(ctx.newSymbol(
          ctx.owner, tree.name, tree.mods.flags, new Completer(tree),
          privateWithinClass(tree.mods), tree.pos))
      case imp: Import =>
        ctx.newSymbol(
          ctx.owner, nme.IMPORT, Synthetic, new Completer(tree), NoSymbol, tree.pos)
      case _ =>
        NoSymbol
    }
    if (sym.exists) symOfTree(tree) = sym
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

  class Completer(original: Tree)(implicit ctx: Context) extends LazyType {

    def complete(denot: SymDenotation): Unit = {
      val sym = denot.symbol
      def localContext = ctx.fresh.withOwner(sym)

      def typeSig(defn: Tree): Type = defn match {
        case defn: ValDef =>
          valOrDefDefSig(defn, sym, identity)(localContext)
        case defn: DefDef =>
          val typer1 = new Typer
          nestedTyper(sym) = typer1
          typer1.defDefSig(defn, sym)(localContext.withScope(typer1.scope))
        case defn: TypeDef =>
          typeDefSig(defn, sym)(localContext.withNewScope)
        case defn: ClassDef =>
          classDefSig(defn, sym.asClass)(localContext)
        case imp: Import =>
          val expr1 = typedAhead(imp.expr)
          ImportType(SharedTree(expr1))
      }

      sym.info = typeSig(original)
    }
  }

  def typedAhead(tree: Tree, mode: Mode.Value = Mode.Expr, pt: Type = WildcardType)(implicit ctx: Context): tpd.Tree =
    typedTree.getOrElseUpdate(tree, typer.typed(tree))

  def valOrDefDefSig(defn: ValOrDefDef, sym: Symbol, paramFn: Type => Type)(implicit ctx: Context): Type = {
    val pt =
      if (!defn.tpt.isEmpty) WildcardType
      else {
        lazy val schema = paramFn(WildcardType)
        val site = sym.owner.symTypeRef
        val inherited = {
          ((NoType: Type) /: sym.owner.info.baseClasses.tail) { (tp, cls) =>
            val itpe = cls.info
              .nonPrivateDecl(sym.name)
              .matchingDenotation(site, schema)
              .asSeenFrom(site)
              .info.finalResultType
            tp & itpe
          }
        }
        inherited orElse typedAhead(defn.rhs).tpe
      }
    paramFn(typedAhead(defn.tpt, Mode.Type, pt).tpe)
  }

  def completeParams(params: List[MemberDef])(implicit ctx: Context) = {
    enterSyms(params)
    for (param <- params) typedAhead(param)
  }

  def defDefSig(ddef: DefDef, sym: Symbol)(implicit ctx: Context) = {
    val DefDef(_, name, tparams, vparamss, _, _) = ddef
    completeParams(tparams)
    vparamss foreach completeParams
    val isConstructor = name == nme.CONSTRUCTOR
    val isSecondaryConstructor = isConstructor && sym != sym.owner.primaryConstructor
    def typeParams =
      if (isSecondaryConstructor) sym.owner.primaryConstructor.typeParams
      else tparams map symOfTree
    def wrapMethType(restpe: Type): Type = {
      val monotpe =
        (restpe /: vparamss) { (restpe, params) =>
          val creator =
            if (params.nonEmpty && (params.head.mods is Implicit)) ImplicitMethodType else MethodType
              creator.fromSymbols(params map symOfTree, restpe)
        }
      if (tparams.nonEmpty) PolyType.fromSymbols(typeParams, monotpe)
      else if (vparamss.isEmpty) ExprType(monotpe)
      else monotpe
    }
    if (isConstructor) {
      // set result type tree to unit, but set the current class as result type of the symbol
      typedAhead(ddef.tpt, Mode.Type, defn.UnitType)
      wrapMethType(sym.owner.typeConstructor.appliedTo(typeParams map (_.symRef)))
    }
    else valOrDefDefSig(ddef, sym, wrapMethType)
  }

  def typeDefSig(defn: TypeDef, sym: Symbol)(implicit ctx: Context): Type = {
    completeParams(defn.tparams)
    ???
  }

  def classDefSig(defn: ClassDef, cls: ClassSymbol)(implicit ctx: Context): Type = {

    def parentType(constr: untpd.Tree): Type = {
      val Trees.Select(Trees.New(tpt), _) = TreeInfo.methPart(constr)
      val ptype = typedAhead(tpt, Mode.Type).tpe
      if (ptype.uninstantiatedTypeParams.isEmpty) ptype
      else typedAhead(constr, Mode.Expr).tpe
    }

    def enterSelfSym(name: TermName, tpe: Type): Unit =
      ctx.enter(ctx.newSymbol(ctx.owner, name, Synthetic, tpe, coord = cls.coord))

    val ClassDef(_, _, impl @ Template(_, parents, self, body)) = defn

    val decls = newScope
    val (params, rest) = body span {
      case td: TypeDef => td.mods is ParamOrAccessor
      case _ => false
    }
    enterSyms(params)
    val parentTypes = parents map parentType
    val parentRefs = ctx.normalizeToRefs(parentTypes, cls, decls)
    val selfTypeOpt = if (self.tpt.isEmpty) NoType else typedAhead(self.tpt, Mode.Type).tpe
    if (self.name != nme.WILDCARD)
      enterSelfSym(self.name, selfTypeOpt orElse cls.typeConstructor)
    enterSyms(rest)
    ClassInfo(cls.owner.thisType, cls, parentRefs, decls, selfTypeOpt)
  }
}