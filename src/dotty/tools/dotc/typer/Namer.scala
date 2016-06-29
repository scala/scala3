package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._, Constants._, StdNames._, Scopes._, Denotations._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import ast.desugar, ast.desugar._
import ProtoTypes._
import util.Positions._
import util.{Attachment, SourcePosition, DotClass}
import collection.mutable
import annotation.tailrec
import ErrorReporting._
import tpd.ListOfTreeDecorator
import config.Printers._
import Annotations._
import Inferencing._
import transform.ValueClasses._
import TypeApplications._
import language.implicitConversions

trait NamerContextOps { this: Context =>

  /** Enter symbol into current class, if current class is owner of current context,
   *  or into current scope, if not. Should always be called instead of scope.enter
   *  in order to make sure that updates to class members are reflected in
   *  finger prints.
   */
  def enter(sym: Symbol): Symbol = {
    ctx.owner match {
      case cls: ClassSymbol => cls.enter(sym)
      case _ => this.scope.openForMutations.enter(sym)
    }
    sym
  }

  /** The denotation with the given name in current context */
  def denotNamed(name: Name): Denotation =
    if (owner.isClass)
      if (outer.owner == owner) { // inner class scope; check whether we are referring to self
        if (scope.size == 1) {
          val elem = scope.lastEntry
          if (elem.name == name) return elem.sym.denot // return self
        }
        assert(scope.size <= 1, scope)
        owner.thisType.member(name)
      }
      else // we are in the outermost context belonging to a class; self is invisible here. See inClassContext.
        owner.findMember(name, owner.thisType, EmptyFlags)
    else
      scope.denotsNamed(name).toDenot(NoPrefix)

  /** Either the current scope, or, if the current context owner is a class,
   *  the declarations of the current class.
   */
  def effectiveScope: Scope =
    if (owner != null && owner.isClass) owner.asClass.unforcedDecls
    else scope

  /** The symbol (stored in some typer's symTree) of an enclosing context definition */
  def symOfContextTree(tree: untpd.Tree) = {
    def go(ctx: Context): Symbol = {
      ctx.typeAssigner match {
        case typer: Typer =>
          tree.getAttachment(typer.SymOfTree) match {
            case Some(sym) => sym
            case None =>
              var cx = ctx.outer
              while (cx.typeAssigner eq typer) cx = cx.outer
              go(cx)
          }
        case _ => NoSymbol
      }
    }
    go(this)
  }

  /** Context where `sym` is defined, assuming we are in a nested context. */
  def defContext(sym: Symbol) =
    outersIterator
      .dropWhile(_.owner != sym)
      .dropWhile(_.owner == sym)
      .next

  /** The given type, unless `sym` is a constructor, in which case the
   *  type of the constructed instance is returned
   */
  def effectiveResultType(sym: Symbol, typeParams: List[Symbol], given: Type) =
    if (sym.name == nme.CONSTRUCTOR) sym.owner.typeRef.appliedTo(typeParams map (_.typeRef))
    else given

  /** if isConstructor, make sure it has one non-implicit parameter list */
  def normalizeIfConstructor(paramSymss: List[List[Symbol]], isConstructor: Boolean) =
    if (isConstructor &&
      (paramSymss.isEmpty || paramSymss.head.nonEmpty && (paramSymss.head.head is Implicit)))
      Nil :: paramSymss
    else
      paramSymss

  /** The method type corresponding to given parameters and result type */
  def methodType(typeParams: List[Symbol], valueParamss: List[List[Symbol]], resultType: Type, isJava: Boolean = false)(implicit ctx: Context): Type = {
    val monotpe =
      (valueParamss :\ resultType) { (params, resultType) =>
        val make =
          if (params.nonEmpty && (params.head is Implicit)) ImplicitMethodType
          else if (isJava) JavaMethodType
          else MethodType
        if (isJava)
          for (param <- params)
            if (param.info.isDirectRef(defn.ObjectClass)) param.info = defn.AnyType
        make.fromSymbols(params, resultType)
      }
    if (typeParams.nonEmpty) PolyType.fromSymbols(typeParams, monotpe)
    else if (valueParamss.isEmpty) ExprType(monotpe)
    else monotpe
  }

  /** Find moduleClass/sourceModule in effective scope */
  private def findModuleBuddy(name: Name)(implicit ctx: Context) = {
    val scope = effectiveScope
    val it = scope.lookupAll(name).filter(_ is Module)
    assert(it.hasNext, s"no companion $name in $scope")
    it.next
  }

  /** Add moduleClass or sourceModule functionality to completer
   *  for a module or module class
   */
  def adjustModuleCompleter(completer: LazyType, name: Name) =
    if (name.isTermName)
      completer withModuleClass (_ => findModuleBuddy(name.moduleClassName))
    else
      completer withSourceModule (_ => findModuleBuddy(name.sourceModuleName))
}

/** This class creates symbols from definitions and imports and gives them
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

  val TypedAhead = new Attachment.Key[tpd.Tree]
  val ExpandedTree = new Attachment.Key[Tree]
  val SymOfTree = new Attachment.Key[Symbol]

  /** A partial map from unexpanded member and pattern defs and to their expansions.
   *  Populated during enterSyms, emptied during typer.
   */
  //lazy val expandedTree = new mutable.AnyRefMap[DefTree, Tree]
  /*{
    override def default(tree: DefTree) = tree // can't have defaults on AnyRefMaps :-(
  }*/

  /** A map from expanded MemberDef, PatDef or Import trees to their symbols.
   *  Populated during enterSyms, emptied at the point a typed tree
   *  with the same symbol is created (this can be when the symbol is completed
   *  or at the latest when the tree is typechecked.
   */
  //lazy val symOfTree = new mutable.AnyRefMap[Tree, Symbol]

  /** A map from expanded trees to their typed versions.
   *  Populated when trees are typechecked during completion (using method typedAhead).
   */
  // lazy val typedTree = new mutable.AnyRefMap[Tree, tpd.Tree]

  /** A map from method symbols to nested typers.
   *  Populated when methods are completed. Emptied when they are typechecked.
   *  The nested typer contains new versions of the four maps above including this
   *  one, so that trees that are shared between different DefDefs can be independently
   *  used as indices. It also contains a scope that contains nested parameters.
   */
  lazy val nestedTyper = new mutable.AnyRefMap[Symbol, Typer]

  /** The scope of the typer.
   *  For nested typers this is a place parameters are entered during completion
   *  and where they survive until typechecking. A context with this typer also
   *  has this scope.
   */
  val scope = newScope

  /** The symbol of the given expanded tree. */
  def symbolOfTree(tree: Tree)(implicit ctx: Context): Symbol = {
    val xtree = expanded(tree)
    xtree.getAttachment(TypedAhead) match {
      case Some(ttree) => ttree.symbol
      case none => xtree.attachment(SymOfTree)
    }
  }

  /** The enclosing class with given name; error if none exists */
  def enclosingClassNamed(name: TypeName, pos: Position)(implicit ctx: Context): Symbol = {
    if (name.isEmpty) NoSymbol
    else {
      val cls = ctx.owner.enclosingClassNamed(name)
      if (!cls.exists) ctx.error(s"no enclosing class or object is named $name", pos)
      cls
    }
  }

  /** Record `sym` as the symbol defined by `tree` */
  def recordSym(sym: Symbol, tree: Tree)(implicit ctx: Context): Symbol = {
    val refs = tree.attachmentOrElse(References, Nil)
    if (refs.nonEmpty) {
      tree.removeAttachment(References)
      refs foreach (_.pushAttachment(OriginalSymbol, sym))
    }
    tree.pushAttachment(SymOfTree, sym)
    sym
  }

  /** If this tree is a member def or an import, create a symbol of it
   *  and store in symOfTree map.
   */
  def createSymbol(tree: Tree)(implicit ctx: Context): Symbol = {

    def privateWithinClass(mods: Modifiers) =
      enclosingClassNamed(mods.privateWithin, mods.pos)

    def checkFlags(flags: FlagSet) =
      if (flags.isEmpty) flags
      else {
        val (ok, adapted, kind) = tree match {
          case tree: TypeDef => (flags.isTypeFlags, flags.toTypeFlags, "type")
          case _ => (flags.isTermFlags, flags.toTermFlags, "value")
        }
        if (!ok)
          ctx.error(i"modifier(s) `$flags' incompatible with $kind definition", tree.pos)
        adapted
      }

    /** Add moduleClass/sourceModule to completer if it is for a module val or class */
    def adjustIfModule(completer: LazyType, tree: MemberDef) =
      if (tree.mods is Module) ctx.adjustModuleCompleter(completer, tree.name.encode)
      else completer

    typr.println(i"creating symbol for $tree in ${ctx.mode}")

    def checkNoConflict(name: Name): Name = {
      def errorName(msg: => String) = {
        ctx.error(msg, tree.pos)
        name.freshened
      }
      def preExisting = ctx.effectiveScope.lookup(name)
      if (ctx.owner is PackageClass)
        if (preExisting.isDefinedInCurrentRun)
          errorName(s"${preExisting.showLocated} is compiled twice")
        else name
      else
        if ((!ctx.owner.isClass || name.isTypeName) && preExisting.exists)
          errorName(i"$name is already defined as $preExisting")
        else name
    }

    val inSuperCall = if (ctx.mode is Mode.InSuperCall) InSuperCall else EmptyFlags
    tree match {
      case tree: TypeDef if tree.isClassDef =>
        val name = checkNoConflict(tree.name.encode).asTypeName
        val flags = checkFlags(tree.mods.flags &~ Implicit)
        val cls = recordSym(ctx.newClassSymbol(
          ctx.owner, name, flags | inSuperCall,
          cls => adjustIfModule(new ClassCompleter(cls, tree)(ctx), tree),
          privateWithinClass(tree.mods), tree.pos, ctx.source.file), tree)
        cls.completer.asInstanceOf[ClassCompleter].init()
        cls
      case tree: MemberDef =>
        val name = checkNoConflict(tree.name.encode)
        val flags = checkFlags(tree.mods.flags)
        val isDeferred = lacksDefinition(tree)
        val deferred = if (isDeferred) Deferred else EmptyFlags
        val method = if (tree.isInstanceOf[DefDef]) Method else EmptyFlags
        val inSuperCall1 = if (tree.mods is ParamOrAccessor) EmptyFlags else inSuperCall
          // suppress inSuperCall for constructor parameters
        val higherKinded = tree match {
          case tree: TypeDef if tree.tparams.nonEmpty && isDeferred => HigherKinded
          case _ => EmptyFlags
        }

        // to complete a constructor, move one context further out -- this
        // is the context enclosing the class. Note that the context in which a
        // constructor is recorded and the context in which it is completed are
        // different: The former must have the class as owner (because the
        // constructor is owned by the class), the latter must not (because
        // constructor parameters are interpreted as if they are outside the class).
        // Don't do this for Java constructors because they need to see the import
        // of the companion object, and it is not necessary for them because they
        // have no implementation.
        val cctx = if (tree.name == nme.CONSTRUCTOR && !(tree.mods is JavaDefined)) ctx.outer else ctx

        recordSym(ctx.newSymbol(
          ctx.owner, name, flags | deferred | method | higherKinded | inSuperCall1,
          adjustIfModule(new Completer(tree)(cctx), tree),
          privateWithinClass(tree.mods), tree.pos), tree)
      case tree: Import =>
        recordSym(ctx.newSymbol(
          ctx.owner, nme.IMPORT, Synthetic, new Completer(tree), NoSymbol, tree.pos), tree)
      case _ =>
        NoSymbol
    }
  }

   /** If `sym` exists, enter it in effective scope. Check that
    *  package members are not entered twice in the same run.
    */
  def enterSymbol(sym: Symbol)(implicit ctx: Context) = {
    if (sym.exists) {
      typr.println(s"entered: $sym in ${ctx.owner} and ${ctx.effectiveScope}")
      ctx.enter(sym)
    }
    sym
  }

  /** Create package if it does not yet exist. */
  private def createPackageSymbol(pid: RefTree)(implicit ctx: Context): Symbol = {
    val pkgOwner = pid match {
      case Ident(_) => if (ctx.owner eq defn.EmptyPackageClass) defn.RootClass else ctx.owner
      case Select(qual: RefTree, _) => createPackageSymbol(qual).moduleClass
    }
    val existing = pkgOwner.info.decls.lookup(pid.name)
    if ((existing is Package) && (pkgOwner eq existing.owner)) existing
    else ctx.newCompletePackageSymbol(pkgOwner, pid.name.asTermName).entered
  }

  /** Expand tree and store in `expandedTree` */
  def expand(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case mdef: DefTree =>
      val expanded = desugar.defTree(mdef)
      typr.println(i"Expansion: $mdef expands to $expanded")
      if (expanded ne mdef) mdef.pushAttachment(ExpandedTree, expanded)
    case _ =>
  }

  /** The expanded version of this tree, or tree itself if not expanded */
  def expanded(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case ddef: DefTree => ddef.attachmentOrElse(ExpandedTree, ddef)
    case _ => tree
  }

  /** A new context that summarizes an import statement */
  def importContext(sym: Symbol, selectors: List[Tree])(implicit ctx: Context) =
    ctx.fresh.setImportInfo(new ImportInfo(sym, selectors))

  /** A new context for the interior of a class */
  def inClassContext(selfInfo: DotClass /* Should be Type | Symbol*/)(implicit ctx: Context): Context = {
    val localCtx: Context = ctx.fresh.setNewScope
    selfInfo match {
      case sym: Symbol if sym.exists && sym.name != nme.WILDCARD =>
        localCtx.scope.openForMutations.enter(sym)
      case _ =>
    }
    localCtx
  }

   /** For all class definitions `stat` in `xstats`: If the companion class if not also defined
   *  in `xstats`, invalidate it by setting its info to NoType.
   */
  def invalidateCompanions(pkg: Symbol, xstats: List[untpd.Tree])(implicit ctx: Context): Unit = {
    val definedNames = xstats collect { case stat: NameTree => stat.name }
    def invalidate(name: TypeName) =
      if (!(definedNames contains name)) {
        val member = pkg.info.decl(name).asSymDenotation
        if (member.isClass && !(member is Package)) member.info = NoType
      }
    xstats foreach {
      case stat: TypeDef if stat.isClassDef =>
        invalidate(stat.name.moduleClassName)
      case _ =>
    }
  }

  /** Expand tree and create top-level symbols for statement and enter them into symbol table */
  def index(stat: Tree)(implicit ctx: Context): Context = {
    expand(stat)
    indexExpanded(stat)
  }

  /** Create top-level symbols for all statements in the expansion of this statement and
   *  enter them into symbol table
   */
  def indexExpanded(stat: Tree)(implicit ctx: Context): Context = expanded(stat) match {
    case pcl: PackageDef =>
      val pkg = createPackageSymbol(pcl.pid)
      index(pcl.stats)(ctx.fresh.setOwner(pkg.moduleClass))
      invalidateCompanions(pkg, Trees.flatten(pcl.stats map expanded))
      setDocstring(pkg, stat)
      ctx
    case imp: Import =>
      importContext(createSymbol(imp), imp.selectors)
    case mdef: DefTree =>
      val sym = enterSymbol(createSymbol(mdef))
      setDocstring(sym, stat)
      ctx
    case stats: Thicket =>
      for (tree <- stats.toList) {
        val sym = enterSymbol(createSymbol(tree))
        setDocstring(sym, stat)
      }
      ctx
    case _ =>
      ctx
  }

  def setDocstring(sym: Symbol, tree: Tree)(implicit ctx: Context) = tree match {
    case t: MemberDef => ctx.base.addDocstring(sym, t.rawComment)
    case _ => ()
  }

  /** Create top-level symbols for statements and enter them into symbol table */
  def index(stats: List[Tree])(implicit ctx: Context): Context = {

    val classDef  = mutable.Map[TypeName, TypeDef]()
    val moduleDef = mutable.Map[TypeName, TypeDef]()

    /** Merge the definitions of a synthetic companion generated by a case class
     *  and the real companion, if both exist.
     */
    def mergeCompanionDefs() = {
      for (cdef @ TypeDef(name, _) <- stats)
        if (cdef.isClassDef) {
          classDef(name) = cdef
          cdef.attachmentOrElse(ExpandedTree, cdef) match {
            case Thicket(cls :: mval :: (mcls @ TypeDef(_, _: Template)) :: crest) =>
              moduleDef(name) = mcls
            case _ =>
          }
        }
      for (mdef @ ModuleDef(name, _) <- stats if !mdef.mods.is(Flags.Package)) {
        val typName = name.toTypeName
        val Thicket(vdef :: (mcls @ TypeDef(_, impl: Template)) :: Nil) = mdef.attachment(ExpandedTree)
        moduleDef(typName) = mcls
        classDef get name.toTypeName match {
          case Some(cdef) =>
            cdef.attachmentOrElse(ExpandedTree, cdef) match {
              case Thicket(cls :: mval :: TypeDef(_, compimpl: Template) :: crest) =>
                val mcls1 = cpy.TypeDef(mcls)(
                    rhs = cpy.Template(impl)(body = compimpl.body ++ impl.body))
                mdef.putAttachment(ExpandedTree, Thicket(vdef :: mcls1 :: Nil))
                moduleDef(typName) = mcls1
                cdef.putAttachment(ExpandedTree, Thicket(cls :: crest))
              case _ =>
            }
          case none =>
        }
      }
    }

    def createLinks(classTree: TypeDef, moduleTree: TypeDef)(implicit ctx: Context) = {
      val claz = ctx.denotNamed(classTree.name.encode).symbol
      val modl = ctx.denotNamed(moduleTree.name.encode).symbol
      ctx.synthesizeCompanionMethod(nme.COMPANION_CLASS_METHOD, claz, modl).entered
      ctx.synthesizeCompanionMethod(nme.COMPANION_MODULE_METHOD, modl, claz).entered
    }

    def createCompanionLinks(implicit ctx: Context): Unit = {
      for (cdef @ TypeDef(name, _) <- classDef.values) {
        moduleDef.getOrElse(name, EmptyTree) match {
          case t: TypeDef =>
            createLinks(cdef, t)
          case EmptyTree =>
        }
      }
    }

    stats foreach expand
    mergeCompanionDefs()
    val ctxWithStats = (ctx /: stats) ((ctx, stat) => indexExpanded(stat)(ctx))
    createCompanionLinks(ctxWithStats)
    ctxWithStats
  }

  /** The completer of a symbol defined by a member def or import (except ClassSymbols) */
  class Completer(val original: Tree)(implicit ctx: Context) extends TypeParamsCompleter {

    protected def localContext(owner: Symbol) = ctx.fresh.setOwner(owner).setTree(original)

    private var myTypeParams: List[TypeSymbol] = null
    private var nestedCtx: Context = null

    def completerTypeParams(sym: Symbol): List[TypeSymbol] = {
      if (myTypeParams == null) {
        //println(i"completing type params of $sym in ${sym.owner}")
        myTypeParams = original match {
          case tdef: TypeDef =>
            nestedCtx = localContext(sym).setNewScope
            locally {
              implicit val ctx: Context = nestedCtx
              completeParams(tdef.tparams)
              tdef.tparams.map(symbolOfTree(_).asType)
            }
          case _ =>
            Nil
        }
      }
      myTypeParams
    }

    private def typeSig(sym: Symbol): Type = original match {
      case original: ValDef =>
        if (sym is Module) moduleValSig(sym)
        else valOrDefDefSig(original, sym, Nil, Nil, identity)(localContext(sym).setNewScope)
      case original: DefDef =>
        val typer1 = ctx.typer.newLikeThis
        nestedTyper(sym) = typer1
        typer1.defDefSig(original, sym)(localContext(sym).setTyper(typer1))
      case original: TypeDef =>
        assert(!original.isClassDef)
        typeDefSig(original, sym, completerTypeParams(sym))(nestedCtx)
      case imp: Import =>
        try {
          val expr1 = typedAheadExpr(imp.expr, AnySelectionProto)
          ImportType(expr1)
        } catch {
          case ex: CyclicReference =>
            typr.println(s"error while completing ${imp.expr}")
            throw ex
        }
    }

    final override def complete(denot: SymDenotation)(implicit ctx: Context) = {
      if (completions != noPrinter && ctx.typerState != this.ctx.typerState) {
        completions.println(completions.getClass.toString)
        def levels(c: Context): Int =
          if (c.typerState eq this.ctx.typerState) 0
          else if (c.typerState == null) -1
          else if (c.outer.typerState == c.typerState) levels(c.outer)
          else levels(c.outer) + 1
        completions.println(s"!!!completing ${denot.symbol.showLocated} in buried typerState, gap = ${levels(ctx)}")
      }
      completeInCreationContext(denot)
    }

    protected def addAnnotations(denot: SymDenotation): Unit = original match {
      case original: untpd.MemberDef =>
        for (annotTree <- untpd.modsDeco(original).mods.annotations) {
          val cls = typedAheadAnnotation(annotTree)
          val ann = Annotation.deferred(cls, implicit ctx => typedAnnotation(annotTree))
          denot.addAnnotation(ann)
        }
      case _ =>
    }

    /** Intentionally left without `implicit ctx` parameter. We need
     *  to pick up the context at the point where the completer was created.
     */
    def completeInCreationContext(denot: SymDenotation): Unit = {
      addAnnotations(denot)
      denot.info = typeSig(denot.symbol)
      Checking.checkWellFormed(denot.symbol)
    }
  }

  class ClassCompleter(cls: ClassSymbol, original: TypeDef)(ictx: Context) extends Completer(original)(ictx) {
    withDecls(newScope)

    protected implicit val ctx: Context = localContext(cls).setMode(ictx.mode &~ Mode.InSuperCall)

    val TypeDef(name, impl @ Template(constr, parents, self, _)) = original

    val (params, rest) = impl.body span {
      case td: TypeDef => td.mods is Param
      case vd: ValDef => vd.mods is ParamAccessor
      case _ => false
    }

    def init() = index(params)

    /** The type signature of a ClassDef with given symbol */
    override def completeInCreationContext(denot: SymDenotation): Unit = {

      /* The type of a parent constructor. Types constructor arguments
       * only if parent type contains uninstantiated type parameters.
       */
      def parentType(parent: untpd.Tree)(implicit ctx: Context): Type =
        if (parent.isType) {
          typedAheadType(parent).tpe
        } else {
          val (core, targs) = stripApply(parent) match {
            case TypeApply(core, targs) => (core, targs)
            case core => (core, Nil)
          }
          val Select(New(tpt), nme.CONSTRUCTOR) = core
          val targs1 = targs map (typedAheadType(_))
          val ptype = typedAheadType(tpt).tpe appliedTo targs1.tpes
          if (ptype.typeParams.isEmpty) ptype
          else typedAheadExpr(parent).tpe
        }

      /* Check parent type tree `parent` for the following well-formedness conditions:
       * (1) It must be a class type with a stable prefix (@see checkClassTypeWithStablePrefix)
       * (2) If may not derive from itself
       * (3) Overriding type parameters must be correctly forwarded. (@see checkTypeParamOverride)
       */
      def checkedParentType(parent: untpd.Tree, paramAccessors: List[Symbol]): Type = {
        val ptype = parentType(parent)(ctx.superCallContext)
        if (cls.isRefinementClass) ptype
        else {
          val pt = checkClassType(ptype, parent.pos,
              traitReq = parent ne parents.head, stablePrefixReq = true)
          if (pt.derivesFrom(cls)) {
            val addendum = parent match {
              case Select(qual: Super, _) if ctx.scala2Mode =>
                "\n(Note that inheriting a class of the same name is no longer allowed)"
              case _ => ""
            }
            ctx.error(i"cyclic inheritance: $cls extends itself$addendum", parent.pos)
            defn.ObjectType
          }
          else if (!paramAccessors.forall(checkTypeParamOverride(pt, _)))
            defn.ObjectType
          else pt
        }
      }

      /* Check that every parameter with the same name as a visible named parameter in the parent
       * class satisfies the following two conditions:
       *  (1) The overriding parameter is also named (i.e. not local/name mangled).
       *  (2) The overriding parameter is passed on directly to the parent parameter, or the
       *      parent parameter is not fully defined.
       * @return true if conditions are satisfied, false otherwise.
       */
      def checkTypeParamOverride(parent: Type, paramAccessor: Symbol): Boolean = {
        var ok = true
        val pname = paramAccessor.name

        def illegal(how: String): Unit = {
          ctx.error(d"Illegal override of public type parameter $pname in $parent$how", paramAccessor.pos)
          ok = false
        }

        def checkAlias(tp: Type): Unit = tp match {
          case tp: RefinedType =>
            if (tp.refinedName == pname)
              tp.refinedInfo match {
                case TypeAlias(alias) =>
                  alias match {
                    case TypeRef(pre, name1) if name1 == pname && (pre =:= cls.thisType) =>
                      // OK, parameter is passed on directly
                    case _ =>
                      illegal(d".\nParameter is both redeclared and instantiated with $alias.")
                  }
                case _ => // OK, argument is not fully defined
              }
            else checkAlias(tp.parent)
          case _ =>
        }
        if (parent.nonPrivateMember(paramAccessor.name).symbol.is(Param))
          if (paramAccessor is Private)
            illegal("\nwith private parameter. Parameter definition needs to be prefixed with `type'.")
          else
            checkAlias(parent)
        ok
      }

      addAnnotations(denot)

      val selfInfo =
        if (self.isEmpty) NoType
        else if (cls.is(Module)) {
          val moduleType = cls.owner.thisType select sourceModule
          if (self.name == nme.WILDCARD) moduleType
          else recordSym(
            ctx.newSymbol(cls, self.name, self.mods.flags, moduleType, coord = self.pos),
            self)
        }
        else createSymbol(self)

      // pre-set info, so that parent types can refer to type params
      denot.info = ClassInfo(cls.owner.thisType, cls, Nil, decls, selfInfo)

      // Ensure constructor is completed so that any parameter accessors
      // which have type trees deriving from its parameters can be
      // completed in turn. Note that parent types access such parameter
      // accessors, that's why the constructor needs to be completed before
      // the parent types are elaborated.
      index(constr)
      symbolOfTree(constr).ensureCompleted()

      val tparamAccessors = decls.filter(_ is TypeParamAccessor).toList
      val parentTypes = ensureFirstIsClass(parents.map(checkedParentType(_, tparamAccessors)))
      val parentRefs = ctx.normalizeToClassRefs(parentTypes, cls, decls)
      typr.println(s"completing $denot, parents = $parents, parentTypes = $parentTypes, parentRefs = $parentRefs")

      index(rest)(inClassContext(selfInfo))
      denot.info = ClassInfo(cls.owner.thisType, cls, parentRefs, decls, selfInfo)
      Checking.checkWellFormed(cls)
      if (isDerivedValueClass(cls)) cls.setFlag(Final)
      cls.setApplicableFlags(
        (NoInitsInterface /: impl.body)((fs, stat) => fs & defKind(stat)))
    }
  }

  /** Typecheck tree during completion, and remember result in typedtree map */
  private def typedAheadImpl(tree: Tree, pt: Type)(implicit ctx: Context): tpd.Tree = {
    val xtree = expanded(tree)
    xtree.getAttachment(TypedAhead) match {
      case Some(ttree) => ttree
      case none =>
        val ttree = typer.typed(tree, pt)
        xtree.pushAttachment(TypedAhead, ttree)
        ttree
    }
  }

  def typedAheadType(tree: Tree, pt: Type = WildcardType)(implicit ctx: Context): tpd.Tree =
    typedAheadImpl(tree, pt)(ctx retractMode Mode.PatternOrType addMode Mode.Type)

  def typedAheadExpr(tree: Tree, pt: Type = WildcardType)(implicit ctx: Context): tpd.Tree =
    typedAheadImpl(tree, pt)(ctx retractMode Mode.PatternOrType)

  def typedAheadAnnotation(tree: Tree)(implicit ctx: Context): Symbol = tree match {
    case Apply(fn, _) => typedAheadAnnotation(fn)
    case TypeApply(fn, _) => typedAheadAnnotation(fn)
    case Select(qual, nme.CONSTRUCTOR) => typedAheadAnnotation(qual)
    case New(tpt) => typedAheadType(tpt).tpe.classSymbol
  }

  /** Enter and typecheck parameter list */
  def completeParams(params: List[MemberDef])(implicit ctx: Context) = {
    index(params)
    for (param <- params) typedAheadExpr(param)
  }

  /** The signature of a module valdef.
   *  This will compute the corresponding module class TypeRef immediately
   *  without going through the defined type of the ValDef. This is necessary
   *  to avoid cyclic references involving imports and module val defs.
   */
  def moduleValSig(sym: Symbol)(implicit ctx: Context): Type = {
    val clsName = sym.name.moduleClassName
    val cls = ctx.denotNamed(clsName) suchThat (_ is ModuleClass)
    ctx.owner.thisType select (clsName, cls)
  }

  /** The type signature of a ValDef or DefDef
   *  @param mdef     The definition
   *  @param sym      Its symbol
   *  @param paramFn  A wrapping function that produces the type of the
   *                  defined symbol, given its final return type
   */
  def valOrDefDefSig(mdef: ValOrDefDef, sym: Symbol, typeParams: List[Symbol], paramss: List[List[Symbol]], paramFn: Type => Type)(implicit ctx: Context): Type = {

    def inferredType = {
      /** A type for this definition that might be inherited from elsewhere:
       *  If this is a setter parameter, the corresponding getter type.
       *  If this is a class member, the conjunction of all result types
       *  of overridden methods.
       *  NoType if neither case holds.
       */
      val inherited =
        if (sym.owner.isTerm) NoType
        else {
          // TODO: Look only at member of supertype instead?
          lazy val schema = paramFn(WildcardType)
          val site = sym.owner.thisType
          ((NoType: Type) /: sym.owner.info.baseClasses.tail) { (tp, cls) =>
            val iRawInfo =
              cls.info.nonPrivateDecl(sym.name).matchingDenotation(site, schema).info
            val iInstInfo = iRawInfo match {
              case iRawInfo: PolyType =>
                if (iRawInfo.paramNames.length == typeParams.length)
                  iRawInfo.instantiate(typeParams map (_.typeRef))
                else NoType
              case _ =>
                if (typeParams.isEmpty) iRawInfo
                else NoType
            }
            val iResType = iInstInfo.finalResultType.asSeenFrom(site, cls)
            if (iResType.exists)
              typr.println(i"using inherited type for ${mdef.name}; raw: $iRawInfo, inst: $iInstInfo, inherited: $iResType")
            tp & iResType
          }
        }

      /** The proto-type to be used when inferring the result type from
       *  the right hand side. This is `WildcardType` except if the definition
       *  is a default getter. In that case, the proto-type is the type of
       *  the corresponding parameter where bound parameters are replaced by
       *  Wildcards.
       */
      def rhsProto = {
        val name = sym.asTerm.name
        val idx = name.defaultGetterIndex
        if (idx < 0) WildcardType
        else {
          val original = name.defaultGetterToMethod
          val meth: Denotation =
            if (original.isConstructorName && (sym.owner is ModuleClass))
              sym.owner.companionClass.info.decl(nme.CONSTRUCTOR)
            else
              ctx.defContext(sym).denotNamed(original)
          def paramProto(paramss: List[List[Type]], idx: Int): Type = paramss match {
            case params :: paramss1 =>
              if (idx < params.length) wildApprox(params(idx))
              else paramProto(paramss1, idx - params.length)
            case nil =>
              WildcardType
          }
          val defaultAlts = meth.altsWith(_.hasDefaultParams)
          if (defaultAlts.length == 1)
            paramProto(defaultAlts.head.info.widen.paramTypess, idx)
          else
            WildcardType
        }
      }

      // println(s"final inherited for $sym: ${inherited.toString}") !!!
      // println(s"owner = ${sym.owner}, decls = ${sym.owner.info.decls.show}")
      def isInline = sym.is(Final, butNot = Method)

      // Widen rhs type and approximate `|' but keep ConstantTypes if
      // definition is inline (i.e. final in Scala2).
      def widenRhs(tp: Type): Type = tp.widenTermRefExpr match {
        case tp: ConstantType if isInline => tp
        case _ => tp.widen.approximateUnion
      }

      // Replace aliases to Unit by Unit itself. If we leave the alias in
      // it would be erased to BoxedUnit.
      def dealiasIfUnit(tp: Type) = if (tp.isRef(defn.UnitClass)) defn.UnitType else tp

      val rhsCtx = ctx.addMode(Mode.InferringReturnType)
      def rhsType = typedAheadExpr(mdef.rhs, inherited orElse rhsProto)(rhsCtx).tpe
      def cookedRhsType = ctx.deskolemize(dealiasIfUnit(widenRhs(rhsType)))
      lazy val lhsType = fullyDefinedType(cookedRhsType, "right-hand side", mdef.pos)
      //if (sym.name.toString == "y") println(i"rhs = $rhsType, cooked = $cookedRhsType")
      if (inherited.exists)
        if (sym.is(Final, butNot = Method) && lhsType.isInstanceOf[ConstantType])
          lhsType // keep constant types that fill in for a non-constant (to be revised when inline has landed).
        else inherited
      else {
        if (sym is Implicit) {
          val resStr = if (mdef.isInstanceOf[DefDef]) "result " else ""
          ctx.error(d"${resStr}type of implicit definition needs to be given explicitly", mdef.pos)
          sym.resetFlag(Implicit)
        }
        lhsType orElse WildcardType
      }
    }

    val tptProto = mdef.tpt match {
      case _: untpd.DerivedTypeTree =>
        WildcardType
      case TypeTree(untpd.EmptyTree) =>
        inferredType
      case TypedSplice(tpt: TypeTree) if !isFullyDefined(tpt.tpe, ForceDegree.none) =>
        val rhsType = typedAheadExpr(mdef.rhs, tpt.tpe).tpe
        mdef match {
          case mdef: DefDef if mdef.name == nme.ANON_FUN =>
            val hygienicType = avoid(rhsType, paramss.flatten)
            if (!(hygienicType <:< tpt.tpe))
              ctx.error(i"return type ${tpt.tpe} of lambda cannot be made hygienic;\n" +
                i"it is not a supertype of the hygienic type $hygienicType", mdef.pos)
            //println(i"lifting $rhsType over $paramss -> $hygienicType = ${tpt.tpe}")
            //println(TypeComparer.explained { implicit ctx => hygienicType <:< tpt.tpe })
          case _ =>
        }
        WildcardType
      case _ =>
        WildcardType
    }
    paramFn(typedAheadType(mdef.tpt, tptProto).tpe)
  }

  /** The type signature of a DefDef with given symbol */
  def defDefSig(ddef: DefDef, sym: Symbol)(implicit ctx: Context) = {
    val DefDef(name, tparams, vparamss, _, _) = ddef
    val isConstructor = name == nme.CONSTRUCTOR

    // The following 3 lines replace what was previously just completeParams(tparams).
    // But that can cause bad bounds being computed, as witnessed by
    // tests/pos/paramcycle.scala. The problematic sequence is this:
    //   0. Class constructor gets completed.
    //   1. Type parameter CP of constructor gets completed
    //   2. As a first step CP's bounds are set to Nothing..Any.
    //   3. CP's real type bound demands the completion of corresponding type parameter DP
    //      of enclosing class.
    //   4. Type parameter DP has a rhs a DerivedFromParam tree, as installed by
    //      desugar.classDef
    //   5. The completion of DP then copies the current bounds of CP, which are still Nothing..Any.
    //   6. The completion of CP finishes installing the real type bounds.
    // Consequence: CP ends up with the wrong bounds!
    // To avoid this we always complete type parameters of a class before the type parameters
    // of the class constructor, but after having indexed the constructor parameters (because
    // indexing is needed to provide a symbol to copy for DP's completion.
    // With the patch, we get instead the following sequence:
    //   0. Class constructor gets completed.
    //   1. Class constructor parameter CP is indexed.
    //   2. Class parameter DP starts completion.
    //   3. Info of CP is computed (to be copied to DP).
    //   4. CP is completed.
    //   5. Info of CP is copied to DP and DP is completed.
    index(tparams)
    if (isConstructor) sym.owner.typeParams.foreach(_.ensureCompleted())
    for (tparam <- tparams) typedAheadExpr(tparam)

    vparamss foreach completeParams
    def typeParams = tparams map symbolOfTree
    val paramSymss = ctx.normalizeIfConstructor(vparamss.nestedMap(symbolOfTree), isConstructor)
    def wrapMethType(restpe: Type): Type = {
      val restpe1 = // try to make anonymous functions non-dependent, so that they can be used in closures
        if (name == nme.ANON_FUN) avoid(restpe, paramSymss.flatten)
        else restpe
      ctx.methodType(tparams map symbolOfTree, paramSymss, restpe1, isJava = ddef.mods is JavaDefined)
    }
    if (isConstructor) {
      // set result type tree to unit, but take the current class as result type of the symbol
      typedAheadType(ddef.tpt, defn.UnitType)
      wrapMethType(ctx.effectiveResultType(sym, typeParams, NoType))
    }
    else valOrDefDefSig(ddef, sym, typeParams, paramSymss, wrapMethType)
  }

  def typeDefSig(tdef: TypeDef, sym: Symbol, tparamSyms: List[TypeSymbol])(implicit ctx: Context): Type = {
    val isDerived = tdef.rhs.isInstanceOf[untpd.DerivedTypeTree]
    //val toParameterize = tparamSyms.nonEmpty && !isDerived
    //val needsLambda = sym.allOverriddenSymbols.exists(_ is HigherKinded) && !isDerived
    def abstracted(tp: Type): Type =
      if (tparamSyms.nonEmpty && !isDerived) tp.LambdaAbstract(tparamSyms)
      //else if (toParameterize) tp.parameterizeWith(tparamSyms)
      else tp

    val dummyInfo = abstracted(TypeBounds.empty)
    sym.info = dummyInfo
      // Temporarily set info of defined type T to ` >: Nothing <: Any.
      // This is done to avoid cyclic reference errors for F-bounds.
      // This is subtle: `sym` has now an empty TypeBounds, but is not automatically
      // made an abstract type. If it had been made an abstract type, it would count as an
      // abstract type of its enclosing class, which might make that class an invalid
      // prefix. I verified this would lead to an error when compiling io.ClassPath.
      // A distilled version is in pos/prefix.scala.
      //
      // The scheme critically relies on an implementation detail of isRef, which
      // inspects a TypeRef's info, instead of simply dealiasing alias types.
    val rhsType = abstracted(typedAheadType(tdef.rhs).tpe)
    val unsafeInfo = rhsType match {
      case bounds: TypeBounds => bounds
      case alias => TypeAlias(alias, if (sym is Local) sym.variance else 0)
    }
    if (isDerived) sym.info = unsafeInfo
    else {
      sym.info = NoCompleter
      sym.info = checkNonCyclic(sym, unsafeInfo, reportErrors = true)
    }

    // Here we pay the price for the cavalier setting info to TypeBounds.empty above.
    // We need to compensate by invalidating caches in references that might
    // still contain the TypeBounds.empty. If we do not do this, stdlib factories
    // fail with a bounds error in PostTyper.
    def ensureUpToDate(tp: Type, outdated: Type) = tp match {
      case tref: TypeRef if tref.info == outdated && sym.info != outdated =>
        tref.uncheckedSetSym(null)
      case _ =>
    }
    ensureUpToDate(sym.typeRef, dummyInfo)
    ensureUpToDate(sym.typeRef.appliedTo(tparamSyms.map(_.typeRef)), TypeBounds.empty)

    etaExpandArgs.apply(sym.info)
  }

  /** Eta expand all class types C appearing as arguments to a higher-kinded
   *  type parameter to type lambdas, e.g. [HK0] => C[HK0]. This is necessary
   *  because in `typedAppliedTypeTree` we might have missed some eta expansions
   *  of arguments in F-bounds, because the recursive type was initialized with
   *  TypeBounds.empty.
   */
  def etaExpandArgs(implicit ctx: Context) = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: RefinedType =>
        val args = tp.argInfos.mapconserve(this)
        if (args.nonEmpty) {
          val tycon = tp.withoutArgs(args)
          val tycon1 = this(tycon)
          val tparams = tycon.typeParamSymbols
          val args1 = if (args.length == tparams.length) etaExpandIfHK(tparams, args) else args
          if ((tycon1 eq tycon) && (args1 eq args)) tp else tycon1.appliedTo(args1)
        } else mapOver(tp)
      case _ => mapOver(tp)
    }
  }
}
