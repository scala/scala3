package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._, Constants._, StdNames._, Scopes._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import util.Positions._
import util.SourcePosition
import collection.mutable
import language.implicitConversions

trait NamerContextOps { ctx: Context =>

  def enterSym(sym: Symbol) = ctx.owner match {
    case cls: ClassSymbol => cls.enter(sym)
    case _ => this.scope.asInstanceOf[MutableScope].enter(sym)
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

  val expandedTree = new mutable.WeakHashMap[Tree, Tree]

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

  def createSymbol(tree: Tree, original: Tree)(implicit ctx: Context): Symbol = {
    def createSym(name: Name, flags: FlagSet, privateWithin: Symbol) = {
      val sym = ctx.newSymbol(ctx.owner, name, flags, new Completer, privateWithin, original.pos)
      untypedTreeOfSym(sym) = tree
      sym
    }
    tree match {
      case tree: MemberDef =>
        val sym = createSym(tree.name, tree.mods.flags, privateWithinClass(tree.mods))
        ctx.enterSym(sym)
        sym
      case imp: Import =>
        createSym(nme.IMPORT, Synthetic, NoSymbol)
      case _ =>
        NoSymbol
    }
  }

  val synthetic = Modifiers(Synthetic)

  def expansion(tree: Tree)(implicit ctx: Context): Tree = {

    def classTypeRef(cdef: ClassDef) = {
      val tycon = Ident(cdef.name)
      if (cdef.tparams.isEmpty) tycon else AppliedTypeTree(tycon, cdef.tparams map refOfDef)
    }

    def creator(cdef: ClassDef) =
      New(classTypeRef(cdef), cdef.impl.constr.vparamss.nestedMap(refOfDef))

    def methTypeParams(cdef: ClassDef) =
      for (tparam <- cdef.tparams) yield // don't use derivedTypeDef; parameters have to be unique
        TypeDef(Modifiers(TypeParam), tparam.name, tparam.tparams, tparam.rhs).withPos(tparam.pos)

    def methParamss(cdef: ClassDef) =
      cdef.impl.constr.vparamss.nestedMap(vparam => // don't use derivedValDef; parameters have to be unique
        ValDef(Modifiers(TermParam), vparam.name, vparam.tpt, vparam.rhs).withPos(vparam.pos))

    def expandCaseClass(cdef: ClassDef): Tree = {
      val ClassDef(mods, cname, tparams, impl @ Template(constr, parents, self, stats)) = cdef
      val constr1 =
        if (constr.vparamss.nonEmpty) constr
        else {
          ctx.error("case class needs to have at least one parameter list", cdef.pos)
          constr.derivedDefDef(constr.mods, constr.name, constr.tparams, ListOfNil, constr.tpt, constr.rhs)
        }
      val caseParams = constr1.vparamss.head
      val caseParamsArray = caseParams.toArray
      def syntheticProperty(name: TermName, rhs: Tree) = DefDef(synthetic, name, Nil, Nil, EmptyTree, rhs)
      val isDefinedMeth = syntheticProperty(nme.isDefined, Literal(Constant(true)))
      val productArityMeth = syntheticProperty(nme.productArity, Literal(Constant(caseParamsArray.length)))
      val productElemMeths = for (i <- 0 until caseParamsArray.length) yield
          syntheticProperty(("_" + (i + 1)).toTermName, Select(This(EmptyTypeName), caseParamsArray(i).name))
      val (copyMeths, applyMeths) =
        if (mods is Abstract) (Nil, Nil)
        else {
          val copyFirstParams = caseParams.map(vparam =>
            ValDef(Modifiers(TermParam), vparam.name, vparam.tpt, refOfDef(vparam)).withPos(vparam.pos))
          val copyRestParamss = constr1.vparamss.tail.nestedMap(vparam =>
            ValDef(Modifiers(TermParam), vparam.name, vparam.tpt, EmptyTree).withPos(vparam.pos))
          val applyParamss = constr1.vparamss.nestedMap(vparam =>
            ValDef(Modifiers(TermParam), vparam.name, vparam.tpt, vparam.rhs).withPos(vparam.pos))
          val copyMeth =
            DefDef(synthetic, nme.copy, methTypeParams(cdef), copyFirstParams :: copyRestParamss, EmptyTree, creator(cdef))
          val applyMeth =
            DefDef(synthetic, nme.apply, methTypeParams(cdef), methParamss(cdef), EmptyTree, creator(cdef))
          (copyMeth :: Nil, applyMeth :: Nil)
        }
      val unapplyMeth = {
        val unapplyParam = makeSyntheticParameter(tpt = classTypeRef(cdef))
        DefDef(synthetic, nme.unapply, methTypeParams(cdef), (unapplyParam :: Nil) :: Nil, EmptyTree, This(EmptyTypeName))
      }
      val classMeths = copyMeths ::: isDefinedMeth :: productArityMeth :: productElemMeths.toList
      val cls1 = addToClass(cdef, classMeths)

      // update or create companion object:
      val companionMeths = applyMeths ::: unapplyMeth :: Nil
      val companionName = cname.toTermName
      var companionFound = false
      for (companion @ ModuleDef(_, `companionName`, _) <- enclosingStats) {
        // Add `companionDefs` to either the expanded or unexpanded version of
        // the companion object with given `companionName`, and update its expandedTree map
        // with the result.
        expandedTree(companion) = expandedTree get companion match {
          case Some(Thicket(vdef :: (cdef: ClassDef) :: Nil)) =>
            Thicket(vdef, addToClass(cdef, companionMeths))
          case none =>
            addToModule(companion, companionMeths)
        }
        companionFound = true
      }
      val syntheticCompanions =
        if (companionFound) Nil
        else {
          val parent =
            if (tparams.nonEmpty) ref(defn.AnyRefAlias.typeConstructor)
            else (constr1.vparamss :\ classTypeRef(cdef)) ((vparams, restpe) =>
              Function(vparams map (_.tpt), restpe))
          ModuleDef(
            Modifiers(Synthetic), companionName,
            Template(emptyConstructor, parent :: Nil, EmptyValDef(), companionMeths)) :: Nil
        }
      Thicket.make(cls1 :: syntheticCompanions)
    }

    def addToTemplate(templ: Template, stats: List[Tree]): Template =
      templ.derivedTemplate(templ.constr, templ.parents, templ.self, templ.body ++ stats)

    def addToClass(cdef: ClassDef, stats: List[Tree]): ClassDef =
      cdef.derivedClassDef(cdef.mods, cdef.name, cdef.tparams, addToTemplate(cdef.impl, stats))

    def addToModule(mdef: ModuleDef, stats: List[Tree]): ModuleDef =
      mdef.derivedModuleDef(mdef.mods, mdef.name, addToTemplate(mdef.impl, stats))

    def implicitWrapper(cdef: ClassDef) =
      DefDef(Modifiers(Synthetic | Implicit), cdef.name.toTermName,
          methTypeParams(cdef), methParamss(cdef), EmptyTree, creator(cdef))

    val tree1 = tree match {
      case ValDef(mods, name, tpt, rhs) =>
        if (!ctx.owner.isClass || (mods is Private)) tree
        else {
          val lname = name.toLocalName
          val field = tree.derivedValDef(mods, lname, tpt, rhs)
          val getter = tree.derivedDefDef(mods, name, Nil, Nil, tpt, Ident(lname))
          if (!(mods is Mutable)) Thicket(field, getter)
          else {
            val setterParam = makeSyntheticParameter(tpt = TypeTree(field))
            val setter = tree.derivedDefDef(
                mods, name.getterToSetter, Nil, (setterParam :: Nil) :: Nil, EmptyTree, refOfDef(setterParam))
            Thicket(field, getter, setter)
          }
        }
      case tdef: TypeDef if tdef.mods is PrivateLocalParamAccessor =>
        val tparam = tdef.derivedTypeDef(
          tdef.mods &~ PrivateLocal | ExpandedName, tdef.name.expandedName(ctx.owner), tdef.tparams, tdef.rhs)
        val alias = tdef.derivedTypeDef(
          Modifiers(PrivateLocal | Synthetic), tdef.name, Nil, refOfDef(tparam))
        Thicket(tparam :: alias :: Nil)
      case mdef: ModuleDef =>
        desugarModuleDef {
          expandedTree get mdef match {
            case Some(mdef1: ModuleDef) => mdef
            case _ => mdef
          }
        }
      case cdef: ClassDef =>
        val cdef1: ClassDef = desugarClassDef(cdef)
        val cdef2 = if (cdef1.mods is Case) expandCaseClass(cdef1) else cdef1
        if (cdef.mods is Implicit) {
          if (ctx.owner is Package)
            ctx.error("implicit classes may not be toplevel", cdef.pos)
          Thicket(cdef2 :: implicitWrapper(cdef) :: Nil)
        }
        else cdef2
      case _ =>
        tree
    }
    if (tree1 ne tree) expandedTree(tree) = tree1
    tree1
  }

  def enterSyms(stats: List[Tree])(implicit ctx: Context): Context = stats match {
    case (imp @ Import(expr, selectors)) :: rest =>
      val sym = createSymbol(imp, imp)
      enterSyms(rest)(ctx.fresh.withImport(ImportInfo(sym, selectors, ctx.scopeNestingLevel)))
    case stat :: rest =>
      for (expanded <- expansion(stat).toList) createSymbol(expanded, stat)
      enterSyms(rest)
    case Nil =>
      ctx
  }

  def localContext(owner: Symbol)(implicit ctx: Context) =
    ctx.fresh.withOwner(owner).withScope(newScope)

  def enterParams(ddef: DefDef)(ctx: Context): Context =
    (enterSyms(ddef.tparams)(ctx) /: ddef.vparamss) ((ctx, params) => enterSyms(params)(ctx))

  class Completer(implicit ctx: Context) extends LazyType {

    def registerTyped(originals: List[NameTree], trees: List[tpd.Tree]): Unit =
      for ((original, tree) <- (originals, trees).zipped)
        typedTreeOfSym(symOfTree(original, untypedTreeOfSym)) = tree

    def complete(denot: SymDenotation): Unit = {
      val sym = denot.symbol
      val original = untypedTreeOfSym(sym)

      def inheritedResultType(paramFn: Type => Type): Type = {
        lazy val schema = paramFn(WildcardType)
        val site = sym.owner.symTypeRef
        ((NoType: Type) /: sym.owner.info.baseClasses.tail) { (tp, cls) =>
          val itpe = cls.info
            .nonPrivateDecl(sym.name)
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

      def valOrDefDefTypeSig[UT <: untpd.ValOrDefDef, T <: tpd.ValOrDefDef]
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

      def defDefTypeSig(defn: DefDef)(implicit ctx: Context) = {
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
          valOrDefDefTypeSig(defn, completeDefDef, wrapMethType)
      }

      def classDefTypeSig(defn: ClassDef)(implicit ctx: Context): Type = {
        val ClassDef(_, _, tparams, impl @ Template(constr, parents, self, body)) = defn
        val localCtx = ctx.fresh.withOwner(sym)
        ???
      }

      def typeSig(defn: Tree): Type = defn match {
        case defn: ValDef =>
          valOrDefDefTypeSig(defn, completeValDef, identity)(ctx.fresh.withOwner(sym))
        case defn: DefDef =>
          defDefTypeSig(defn)(localContext(sym))
        case defn: TypeDef =>
          val localCtx = localContext(sym)
          completeParams(defn.tparams, completeTypeDef)(localCtx)
          val TypeDef(_, _, _, rhs) = aheadDef(defn, completeTypeDef)(localCtx)
          rhs.tpe   // !!! do something about parameters!
        case defn: ClassDef =>
          classDefTypeSig(defn)(ctx.fresh.withOwner(sym))
        case imp: Import =>
          val expr1 = typedDefn(imp.expr, sym)
          ImportType(SharedTree(expr1))
      }

      sym.info = typeSig(original)
    }
  }
}