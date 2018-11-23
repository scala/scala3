package dotty.tools.dotc.interactive

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.config.Printers.interactiv
import dotty.tools.dotc.core.Contexts.{Context, NoContext}
import dotty.tools.dotc.core.CheckRealizable
import dotty.tools.dotc.core.Decorators.StringInterpolators
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.NameKinds.SimpleNameKind
import dotty.tools.dotc.core.NameOps.NameDecorator
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.Scopes
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.core.Types.{NamedType, Type, takeAllFilter}
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition}

import scala.collection.mutable

object Completion {

  import dotty.tools.dotc.ast.tpd._

  /** Get possible completions from tree at `pos`
   *
   *  @return offset and list of symbols for possible completions
   */
  def completions(pos: SourcePosition)(implicit ctx: Context): (Int, List[Symbol]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.pos)
    computeCompletions(pos, path)(Interactive.contextOfPath(path))
  }

  /**
   * Inspect `path` to determine what kinds of symbols should be considered.
   *
   * If the path starts with:
   *  - a `RefTree`, then accept symbols of the same kind as its name;
   *  - a renaming import, and the cursor is on the renamee, accept both terms and types;
   *  - an import, accept both terms and types;
   *
   * Otherwise, provide no completion suggestion.
   */
  private def completionMode(path: List[Tree], pos: SourcePosition): Mode = {
    path match {
      case (ref: RefTree) :: _ =>
        if (ref.name.isTermName) Mode.Term
        else if (ref.name.isTypeName) Mode.Type
        else Mode.None

      case Thicket(name :: _ :: Nil) :: (_: Import) :: _ =>
        if (name.pos.contains(pos.pos)) Mode.Import
        else Mode.None // Can't help completing the renaming

      case Import(_, _) :: _ =>
        Mode.Import

      case _ =>
        Mode.None
    }
  }

  /**
   * Inspect `path` to determine the completion prefix. Only symbols whose name start with the
   * returned prefix should be considered.
   */
  private def completionPrefix(path: List[Tree], pos: SourcePosition): String = {
    path match {
      case Thicket(name :: _ :: Nil) :: (_: Import) :: _ =>
        completionPrefix(name :: Nil, pos)

      case Import(expr, selectors) :: _ =>
        selectors.find(_.pos.contains(pos.pos)).map { selector =>
          completionPrefix(selector.asInstanceOf[Tree] :: Nil, pos)
        }.getOrElse("")

      case (ref: RefTree) :: _ =>
        if (ref.name == nme.ERROR) ""
        else ref.name.toString.take(pos.pos.point - ref.pos.point)

      case _ =>
        ""
    }
  }

  /** Inspect `path` to determine the offset where the completion result should be inserted. */
  private def completionOffset(path: List[Tree]): Int = {
    path match {
      case (ref: RefTree) :: _ => ref.pos.point
      case _ => 0
    }
  }

  /** Create a new `CompletionBuffer` for completing at `pos`. */
  private def completionBuffer(path: List[Tree], pos: SourcePosition): CompletionBuffer = {
    val mode = completionMode(path, pos)
    val prefix = completionPrefix(path, pos)
    new CompletionBuffer(mode, prefix, pos)
  }

  private def computeCompletions(pos: SourcePosition, path: List[Tree])(implicit ctx: Context): (Int, List[Symbol]) = {

    val offset = completionOffset(path)
    val buffer = completionBuffer(path, pos)

    if (buffer.mode != Mode.None) {
      path match {
        case Select(qual, _) :: _                 => buffer.addMemberCompletions(qual)
        case Import(expr, _) :: _                 => buffer.addMemberCompletions(expr)
        case (_: Thicket) :: Import(expr, _) :: _ => buffer.addMemberCompletions(expr)
        case _                                    => buffer.addScopeCompletions
      }
    }

    val completionList = buffer.getCompletions

    interactiv.println(i"""completion with pos     = $pos,
                          |                prefix  = ${buffer.prefix},
                          |                term    = ${buffer.mode.is(Mode.Term)},
                          |                type    = ${buffer.mode.is(Mode.Type)}
                          |                results = $completionList%, %""")
    (offset, completionList)
  }

  private class CompletionBuffer(val mode: Mode, val prefix: String, pos: SourcePosition) {

    private[this] val completions = Scopes.newScope.openForMutations

    /**
     * Return the list of symbols that shoudl be included in completion results.
     *
     * If the mode is `Import` and several symbols share the same name, the type symbols are
     * preferred over term symbols.
     */
    def getCompletions(implicit ctx: Context): List[Symbol] = {
      // Show only the type symbols when there are multiple options with the same name
      completions.toList.groupBy(_.name.stripModuleClassSuffix.toSimpleName).mapValues {
        case sym :: Nil => sym :: Nil
        case syms => syms.filter(_.isType)
      }.values.flatten.toList
    }

    /**
     * Add symbols that are currently in scope to `info`: the members of the current class and the
     * symbols that have been imported.
     */
    def addScopeCompletions(implicit ctx: Context): Unit = {
      if (ctx.owner.isClass) {
        addAccessibleMembers(ctx.owner.thisType)
        ctx.owner.asClass.classInfo.selfInfo match {
          case selfSym: Symbol => add(selfSym)
          case _ =>
        }
      }
      else if (ctx.scope != null) ctx.scope.foreach(add)

      addImportCompletions

      var outer = ctx.outer
      while ((outer.owner `eq` ctx.owner) && (outer.scope `eq` ctx.scope)) {
        addImportCompletions(outer)
        outer = outer.outer
      }
      if (outer `ne` NoContext) addScopeCompletions(outer)
    }

    /**
     * Find all the members of `qual` and add the ones that pass the include filters to `info`.
     *
     * If `info.mode` is `Import`, the members added via implicit conversion on `qual` are not
     * considered.
     */
    def addMemberCompletions(qual: Tree)(implicit ctx: Context): Unit = {
      addAccessibleMembers(qual.tpe)
      if (!mode.is(Mode.Import)) {
        // Implicit conversions do not kick in when importing
        implicitConversionTargets(qual)(ctx.fresh.setExploreTyperState())
          .foreach(addAccessibleMembers)
      }
    }

    /**
     * If `sym` exists, no symbol with the same name is already included, and it satisfies the
     * inclusion filter, then add it to the completions.
     */
    private def add(sym: Symbol)(implicit ctx: Context) =
      if (sym.exists && !completions.lookup(sym.name).exists && include(sym)) {
        completions.enter(sym)
      }

    /** Lookup members `name` from `site`, and try to add them to the completion list. */
    private def addMember(site: Type, name: Name)(implicit ctx: Context) =
      if (!completions.lookup(name).exists)
        for (alt <- site.member(name).alternatives) add(alt.symbol)

    /** Include in completion sets only symbols that
     *   1. start with given name prefix, and
     *   2. do not contain '$' except in prefix where it is explicitly written by user, and
     *   3. are not a primary constructor,
     *   4. are the module class in case of packages,
     *   5. are mutable accessors, to exclude setters for `var`,
     *   6. have same term/type kind as name prefix given so far
     *
     *  The reason for (2) is that we do not want to present compiler-synthesized identifiers
     *  as completion results. However, if a user explicitly writes all '$' characters in an
     *  identifier, we should complete the rest.
     */
    private def include(sym: Symbol)(implicit ctx: Context): Boolean =
      sym.name.startsWith(prefix) &&
      !sym.name.toString.drop(prefix.length).contains('$') &&
      !sym.isPrimaryConstructor &&
      (!sym.is(Package) || !sym.moduleClass.exists) &&
      !sym.is(allOf(Mutable, Accessor)) &&
      (
           (mode.is(Mode.Term) && sym.isTerm)
        || (mode.is(Mode.Type) && (sym.isType || sym.isStable))
      )

    /**
     * Find all the members of `site` that are accessible and which should be included in `info`.
     *
     * @param site The type to inspect.
     * @return The members of `site` that are accessible and pass the include filter of `info`.
     */
    private def accessibleMembers(site: Type)(implicit ctx: Context): Seq[Symbol] = site match {
      case site: NamedType if site.symbol.is(Package) =>
        site.decls.toList.filter(include) // Don't look inside package members -- it's too expensive.
      case _ =>
        def appendMemberSyms(name: Name, buf: mutable.Buffer[SingleDenotation]): Unit =
          try buf ++= site.member(name).alternatives
          catch { case ex: TypeError => }
        site.memberDenots(takeAllFilter, appendMemberSyms).collect {
          case mbr if include(mbr.symbol) => mbr.accessibleFrom(site, superAccess = true).symbol
          case _ => NoSymbol
        }.filter(_.exists)
    }

    /** Add all the accessible members of `site` in `info`. */
    private def addAccessibleMembers(site: Type)(implicit ctx: Context): Unit =
      for (mbr <- accessibleMembers(site)) addMember(site, mbr.name)

    /**
     * Add in `info` the symbols that are imported by `ctx.importInfo`. If this is a wildcard import,
     * all the accessible members of the import's `site` are included.
     */
    private def addImportCompletions(implicit ctx: Context): Unit = {
      val imp = ctx.importInfo
      if (imp != null) {
        def addImport(name: TermName) = {
          addMember(imp.site, name)
          addMember(imp.site, name.toTypeName)
        }
        // FIXME: We need to also take renamed items into account for completions,
        // That means we have to return list of a pairs (Name, Symbol) instead of a list
        // of symbols from `completions`.!=
        for (imported <- imp.originals if !imp.excluded.contains(imported)) addImport(imported)
        if (imp.isWildcardImport)
          for (mbr <- accessibleMembers(imp.site) if !imp.excluded.contains(mbr.name.toTermName))
            addMember(imp.site, mbr.name)
      }
    }

    /**
     * Given `qual` of type T, finds all the types S such that there exists an implicit conversion
     * from T to S.
     *
     * @param qual The argument to which the implicit conversion should be applied.
     * @return The set of types that `qual` can be converted to.
     */
    private def implicitConversionTargets(qual: Tree)(implicit ctx: Context): Set[Type] = {
      val typer = ctx.typer
      val conversions = new typer.ImplicitSearch(defn.AnyType, qual, pos.pos).allImplicits
      val targets = conversions.map(_.widen.finalResultType)
      interactiv.println(i"implicit conversion targets considered: ${targets.toList}%, %")
      targets
    }

  }

  /**
   * The completion mode: defines what kinds of symbols should be included in the completion
   * results.
   */
  private class Mode(val bits: Int) extends AnyVal {
    def is(other: Mode): Boolean = (bits & other.bits) == other.bits
    def |(other: Mode): Mode = new Mode(bits | other.bits)
  }
  private object Mode {
    /** No symbol should be included */
    val None: Mode = new Mode(0)

    /** Term symbols are allowed */
    val Term: Mode = new Mode(1)

    /** Type and stable term symbols are allowed */
    val Type: Mode = new Mode(2)

    /** Both term and type symbols are allowed */
    val Import: Mode = new Mode(4) | Term | Type
  }

}
