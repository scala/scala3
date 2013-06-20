package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._, Constants._, StdNames._, Scopes._, Denotations._
import Contexts._, Symbols._, Types._, SymDenotations._, Names._, NameOps._, Flags._, Decorators._
import util.Positions._
import util.SourcePosition
import collection.mutable
import annotation.tailrec
import language.implicitConversions

trait TyperContextOps { ctx: Context => }

object Typer {

  object BindingPrec {
    val definition = 4
    val namedImport = 3
    val wildImport = 2
    val packageClause = 1
    val nothingBound = 0
    def isImportPrec(prec: Int) = prec == namedImport || prec == wildImport
  }
}

class Typer extends Namer {

  import tpd._
  import Typer.BindingPrec

  def typedSelection(site: Type, name: Name, pos: Position)(implicit ctx: Context): Type = {
    val ref = site.member(name)
    if (ref.exists) NamedType(site, name).withDenot(ref)
    else {
      ctx.error(s"$name is not a member of ${site.show}", pos)
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

  /** Attribute an identifier consisting of a simple name or an outer reference.
   *
   *  @param tree      The tree representing the identifier.
   *  Transformations: (1) Prefix class members with this.
   *                   (2) Change imported symbols to selections
   *
   */
  def typedIdent(tree: untpd.Ident, mode: Mode)(implicit ctx: Context): Tree = {
    val name = tree.name

    /** A symbol qualifies if it exists and is not stale. Stale symbols
     *  are made to disappear here. In addition,
     *  if we are in a constructor of a pattern, we ignore all definitions
     *  which are methods (note: if we don't do that
     *  case x :: xs in class List would return the :: method)
     *  unless they are stable or are accessors (the latter exception is for better error messages)
     */
    def qualifies(sym: Symbol): Boolean = !(
         sym.isAbsent
      || (mode is Mode.Pattern | Mode.Fun) && (sym is (Method, butNot = Accessor))
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
          case Trees.Pair(Trees.Ident(from), Trees.Ident(`name`)) :: rest =>
            checkUnambiguous(typedSelection(site, name, tree.pos))
          case Trees.Ident(`name`) :: rest =>
            checkUnambiguous(typedSelection(site, name, tree.pos))
          case _ :: rest =>
            namedImportRef(site, rest)
          case nil =>
            NoType
        }
      }

      /** The type representing a wildcard import with enclosing name when imported
       *  from given `site` and `selectors`.
       */
      def wildImportRef(site: Type, selectors: List[untpd.Tree]): Type = {
        def wildPermitted(selectors: List[untpd.Tree]): Boolean = selectors match {
          case Trees.Pair(Trees.Ident(`name`), Trees.Ident(nme.WILDCARD)) :: _ => false
          case Trees.Ident(nme.WILDCARD) :: _ => true
          case _ :: rest => wildPermitted(rest)
          case nil => false
        }
        if (wildPermitted(selectors)) {
          val denot = site.member(name)
          if (denot.exists) return NamedType(site, name).withDenot(denot)
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
        val curScope = ctx.scope
        val curOwner = ctx.owner
        if (curScope ne outer.scope) {
          val defDenots =
            if (curOwner.isClass && (curOwner ne outer.owner)) curOwner.asClass.membersNamed(name)
            else curScope.denotsNamed(name)
          if (defDenots.exists) {
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
            val wildImp = wildImportRef(curImport.site, curImport.selectors)
            if (wildImp.exists)
              return findRef(checkNewOrShadowed(wildImp, wildImport), wildImport, ctx)(outer)
          }
        }
        findRef(previous, prevPrec, prevCtx)(outer)
      }
    }

    // begin typedIdent
    val startingContext = // ignore current variable scope in patterns to enforce linearity
      if (mode is Mode.Pattern) ctx.outer else ctx

    val rawType = findRef(NoType, BindingPrec.nothingBound, NoContext)
    val ownType =
      if (rawType.exists) checkAccessible(rawType, superAccess = false, tree.pos)
      else {
        ctx.error(s"not found: $name", tree.pos)
        ErrorType
      }
    tree.withType(ownType)
  }

  def typedModifiers(mods: untpd.Modifiers)(implicit ctx: Context): Modifiers = {
    val annotations1 = mods.annotations mapconserve typedAnnotation
    if (annotations1 eq mods.annotations) mods.asInstanceOf[Modifiers]
    else Trees.Modifiers(mods.flags, mods.privateWithin, annotations1)
  }

  def typedAnnotation(annot: untpd.Tree)(implicit ctx: Context): Tree =
    typed(annot, Mode.Expr, defn.AnnotationClass.typeConstructor)

  def typedValDef(vdef: untpd.ValDef, sym: Symbol)(implicit ctx: Context) = {
    val Trees.ValDef(mods, name, tpt, rhs) = vdef
    val mods1 = typedModifiers(mods)
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    val pt = if (sym.exists) sym.symRef else NoType
    vdef.withType(pt).derivedValDef(mods1, name, tpt1, rhs1)
  }

  def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
    val Trees.DefDef(mods, name, tparams, vparamss, tpt, rhs) = ddef
    val mods1 = typedModifiers(mods)
    val tparams1 = tparams mapconserve (typed(_).asInstanceOf[TypeDef])
    val vparamss1 = vparamss.mapconserve(_ mapconserve (typed(_).asInstanceOf[ValDef]))
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    ddef.withType(sym.symRef).derivedDefDef(mods1, name, tparams1, vparamss1, tpt1, rhs1)
  }

  def typedTypeDef(tdef: untpd.TypeDef, sym: Symbol)(implicit ctx: Context): TypeDef = {
    val Trees.TypeDef(mods, name, rhs) = tdef
    val mods1 = typedModifiers(mods)
    val rhs1 = typedType(rhs)
    tdef.withType(sym.symRef).derivedTypeDef(mods1, name, rhs1)
  }

  def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(implicit ctx: Context) = {
    val Trees.TypeDef(mods, name, impl @ Template(constr, parents, self, body)) = cdef
    val mods1 = typedModifiers(mods)
    val constr1 = typed(constr).asInstanceOf[DefDef]
    val parents1 = parents mapconserve (typed(_))
    val self1 = self.withType(NoType).derivedValDef(
      typedModifiers(self.mods), self.name, typed(self.tpt), EmptyTree)

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
    val expr1 = typed(imp.expr)
    imp.withType(sym.symRef).derivedImport(expr1, imp.selectors)
  }

  def typedExpanded(tree: untpd.Tree, mode: Mode = Mode.Expr, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
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
          if (!tree.isEmpty) typed(tree.original, Mode.Type, pt)
          else {
            assert(pt != WildcardType)
            tree.withType(pt)
          }
        case untpd.EmptyTree =>
          tpd.EmptyTree
      }
    }
  }

  def typed(tree: untpd.Tree, mode: Mode = Mode.Expr, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
    val xtree =
      tree match {
        case tree: untpd.MemberDef =>
          expandedTree remove tree match {
            case Some(xtree) => xtree
            case none => tree
          }
        case _ => tree
    }
    typedExpanded(xtree, mode, pt)
  }

  def typedTrees(trees: List[untpd.Tree], mode: Mode = Mode.Expr)(implicit ctx: Context): List[Tree] =
    trees mapconserve (typed(_, mode))

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
    typed(tree, Mode.Expr, pt)
  def typedType(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, Mode.Type, pt)

}