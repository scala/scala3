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
import annotation.tailrec
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

/** This class attaches creates symbols from definitions and imports and gives them
 *  lazy types.
 *
 *  Timeline:
 *
 *  During enter, trees are expanded as necessary, populating the expandedTree map.
 *  Symbols are created, and the symOfTree map is set up.
 *
 *  Symbol completion causes some trees to be already typechecked and typedTree
 *  entries are created to associate the typed trees with the untyped expanded originals.
 *
 *  During typer, original trees are first expanded using expandedTree. For each
 *  expanded member definition or import we extract and remove the corresponding symbol
 *  from the symOfTree map and complete it. We then consult the typedTree map to see
 *  whether a typed tree exists already. If yes, the typed tree is returned as result.
 *  Otherwise, we proceed with regular type checking.
 *
 *  The scheme is designed to allow sharing of nodes, as long as each duplicate appears
 *  in a different method.
 */
class Namer { typer: Typer =>

  import untpd._


  /** A partial map from unexpanded member defs to their expansions.
   *  Populated during enterSyms, emptied during typer.
   */
  lazy val expandedTree = new mutable.HashMap[MemberDef, Tree]

  /** A map from expanded MemberDef or Import trees to their symbols.
   *  Populated during enterSyms, emptied at the point a typed tree
   *  with the same symbol is created (this can be when the symbol is completed
   *  or at the latest when the tree is typechecked.
   */
  lazy val symOfTree = new mutable.HashMap[Tree, Symbol]

  /** A map from expanded trees their typed versions.
   *  Populated when trees are typechecked during completion (using method typedAhead).
   *  Emptied during typer.
   */
  lazy val typedTree = new mutable.HashMap[Tree, tpd.Tree]

  /** A map from method symbols to nested typers.
   *  Populated when methods are completed. Emptied when they are typechecked.
   *  The nested typer contains new versions of the four maps above including this
   *  one, so that trees that are shared between different DefDefs can be independently
   *  used as indices. It also contains a scope that contains nested parameters.
   */
  lazy val nestedTyper = new mutable.HashMap[Symbol, Typer]

  /** The scope of the typer.
   *  For nested typers this is a place parameters are entered during completion
   *  and where they survive until typechecking.
   */
  val scope = newScope

  /** The symbol of the given expanded tree. */
  def symbolOfTree(tree: Tree)(implicit ctx: Context): Symbol = typedTree get tree match {
    case Some(tree1) => tree1.denot.symbol
    case _ => symOfTree(tree)
  }

  /** The enclosing class with given name; error if none exists */
  def enclosingClassNamed(name: TypeName, pos: Position)(implicit ctx: Context): Symbol = {
    if (name.isEmpty) NoSymbol
    else {
      val cls = ctx.owner.enclosingClassNamed(name)
      if (!cls.exists) ctx.error(s"no enclosing class or object is named name", pos)
      cls
    }
  }

  /** If this tree is a member def or an import, create a symbol of it
   *  and store in symOfTree map.
   */
  def createSymbol(tree: Tree)(implicit ctx: Context): Symbol = {
    def privateWithinClass(mods: Modifiers) =
      enclosingClassNamed(mods.privateWithin, mods.pos)
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

  /** The expansion of a member def */
  def expansion(mdef: MemberDef)(implicit ctx: Context): Tree = {
    val expanded = desugar.memberDef(mdef)
    if (expanded ne mdef) expandedTree(mdef) = expanded
    expanded
  }

  /** A new context that summarizes an import statement */
  def importContext(sym: Symbol, selectors: List[Tree])(implicit ctx: Context) =
    ctx.fresh.withImport(ImportInfo(sym, selectors, ctx.scopeNestingLevel))

  /** A new context for the interior of a class */
  def inClassContext(cls: ClassSymbol, selfName: TermName)(implicit ctx: Context): Context = {
    val localCtx: Context = ctx.fresh.withNewScope
    if (selfName != nme.WILDCARD)
      localCtx.enter(localCtx.newSelfSym(cls, selfName, cls.thisType))
    localCtx
  }

  /** Enter statement */
  def enterSym(stat: Tree)(implicit ctx: Context): Context = stat match {
    case imp: Import =>
      importContext(createSymbol(imp), imp.selectors)
    case mdef: MemberDef =>
      expansion(mdef).toList foreach createSymbol
      ctx
    case _ =>
      ctx
  }

  /** Enter all statements in stats.
   */
  def enterSyms(stats: List[Tree])(implicit ctx: Context): Context = {
    @tailrec def traverse(stats: List[Tree])(implicit ctx: Context): Context = stats match {
      case stat :: stats1 =>
        traverse(stats)(enterSym(stat))
      case nil =>
        ctx
    }

    /** Merge the definitions of a synthetic companion generated by a case class
     *  and the real companion, if both exist.
     */
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

  /** The completer of a symbol defined by a member def or import */
  class Completer(original: Tree)(implicit ctx: Context) extends LazyType {

    def complete(denot: SymDenotation): Unit = {
      val sym = denot.symbol
      def localContext = ctx.fresh.withOwner(sym)

      def typeSig(tree: Tree): Type = tree match {
        case tree: ValDef =>
          valOrDefDefSig(tree, sym, identity)(localContext)
        case tree: DefDef =>
          val typer1 = new Typer
          nestedTyper(sym) = typer1
          typer1.defDefSig(tree, sym)(localContext.withTyper(typer1))
        case tree: TypeDef =>
          typeDefSig(tree, sym)(localContext.withNewScope)
        case tree: ClassDef =>
          classDefSig(tree, sym.asClass)(localContext)
        case imp: Import =>
          val expr1 = typedAhead(imp.expr)
          ImportType(SharedTree(expr1))
      }

      sym.info = typeSig(original)
    }
  }

  /** Typecheck tree during completion, and remember result in yypedtree map */
  def typedAhead(tree: Tree, mode: Mode.Value = Mode.Expr, pt: Type = WildcardType)(implicit ctx: Context): tpd.Tree =
    typedTree.getOrElseUpdate(tree, typer.typedExpanded(tree, mode, pt))

  /** Enter and typecheck parameter list */
  def completeParams(params: List[MemberDef])(implicit ctx: Context) = {
    enterSyms(params)
    for (param <- params) typedAhead(param)
  }

  /** The type signature of a ValDef or DefDef
   *  @param mdef     The definition
   *  @param sym      Its symbol
   *  @param paramFn  A wrapping function that produces the type of the
   *                  defined symbol, given its final return type
   */
  def valOrDefDefSig(mdef: ValOrDefDef, sym: Symbol, paramFn: Type => Type)(implicit ctx: Context): Type = {
    val pt =
      if (!mdef.tpt.isEmpty) WildcardType
      else {
        lazy val schema = paramFn(WildcardType)
        val site = sym.owner.thisType
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
        inherited orElse typedAhead(mdef.rhs).tpe
      }
    paramFn(typedAhead(mdef.tpt, Mode.Type, pt).tpe)
  }

  /** The type signature of a DefDef with given symbol */
  def defDefSig(ddef: DefDef, sym: Symbol)(implicit ctx: Context) = {
    val DefDef(_, name, tparams, vparamss, _, _) = ddef
    completeParams(tparams)
    vparamss foreach completeParams
    val isConstructor = name == nme.CONSTRUCTOR
    val isSecondaryConstructor = isConstructor && sym != sym.owner.primaryConstructor
    def typeParams =
      if (isSecondaryConstructor) sym.owner.primaryConstructor.typeParams
      else tparams map symbolOfTree
    def wrapMethType(restpe: Type): Type = {
      val monotpe =
        (restpe /: vparamss) { (restpe, params) =>
          val make =
            if (params.nonEmpty && (params.head.mods is Implicit)) ImplicitMethodType
            else MethodType
          make.fromSymbols(params map symbolOfTree, restpe)
        }
      if (typeParams.nonEmpty) PolyType.fromSymbols(typeParams, monotpe)
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

  def typeDefSig(tdef: TypeDef, sym: Symbol)(implicit ctx: Context): Type = {
    completeParams(tdef.tparams)
    ???
  }

  /** The type signature of a ClassDef with given symbol */
  def classDefSig(cdef: ClassDef, cls: ClassSymbol)(implicit ctx: Context): Type = {

    def parentType(constr: untpd.Tree): Type = {
      val Trees.Select(Trees.New(tpt), _) = TreeInfo.methPart(constr)
      val ptype = typedAhead(tpt, Mode.Type).tpe
      if (ptype.uninstantiatedTypeParams.isEmpty) ptype
      else typedAhead(constr, Mode.Expr).tpe
    }

    val ClassDef(_, _, impl @ Template(_, parents, self, body)) = cdef

    val decls = newScope
    val (params, rest) = body span {
      case td: TypeDef => td.mods is Param
      case td: ValDef => td.mods is ParamAccessor
      case _ => false
    }
    enterSyms(params)
    val parentTypes = parents map parentType
    val parentRefs = ctx.normalizeToRefs(parentTypes, cls, decls)
    val optSelfType = if (self.tpt.isEmpty) NoType else typedAhead(self.tpt, Mode.Type).tpe
    enterSyms(rest)(inClassContext(cls, self.name))
    ClassInfo(cls.owner.thisType, cls, parentRefs, decls, optSelfType)
  }
}