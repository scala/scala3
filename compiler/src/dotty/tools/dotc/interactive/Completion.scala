package dotty.tools.dotc.interactive

import java.nio.charset.Charset

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
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol, defn}
import dotty.tools.dotc.core.Scopes
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.core.Types.{NameFilter, NamedType, NoType, Type}
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.util.{NameTransformer, NoSourcePosition, SourcePosition}

import scala.collection.mutable

/**
 * One of the results of a completion query.
 *
 * @param label         The label of this completion result, or the text that this completion result
 *                      should insert in the scope where the completion request happened.
 * @param description   The description of this completion result: the fully qualified name for
 *                      types, or the type for terms.
 * @param symbols       The symbols that are matched by this completion result.
 */
case class Completion(label: String, description: String, symbols: List[Symbol])

object Completion {

  import dotty.tools.dotc.ast.tpd._

  /** Get possible completions from tree at `pos`
   *
   *  @return offset and list of symbols for possible completions
   */
  def completions(pos: SourcePosition)(implicit ctx: Context): (Int, List[Completion]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
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
        if (name.span.contains(pos.span)) Mode.Import
        else Mode.None // Can't help completing the renaming

      case Import(_, _, _) :: _ =>
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

      case Import(_, expr, selectors) :: _ =>
        selectors.find(_.span.contains(pos.span)).map { selector =>
          completionPrefix(selector.asInstanceOf[Tree] :: Nil, pos)
        }.getOrElse("")

      case (ref: RefTree) :: _ =>
        if (ref.name == nme.ERROR) ""
        else ref.name.toString.take(pos.span.point - ref.span.point)

      case _ =>
        ""
    }
  }

  /** Inspect `path` to determine the offset where the completion result should be inserted. */
  private def completionOffset(path: List[Tree]): Int = {
    path match {
      case (ref: RefTree) :: _ => ref.span.point
      case _ => 0
    }
  }

  /** Create a new `CompletionBuffer` for completing at `pos`. */
  private def completionBuffer(path: List[Tree], pos: SourcePosition): CompletionBuffer = {
    val mode = completionMode(path, pos)
    val prefix = completionPrefix(path, pos)
    new CompletionBuffer(mode, prefix, pos)
  }

  private def computeCompletions(pos: SourcePosition, path: List[Tree])(implicit ctx: Context): (Int, List[Completion]) = {

    val offset = completionOffset(path)
    val buffer = completionBuffer(path, pos)

    if (buffer.mode != Mode.None) {
      path match {
        case Select(qual, _) :: _                    => buffer.addMemberCompletions(qual)
        case Import(_, expr, _) :: _                 => buffer.addMemberCompletions(expr) // TODO: distinguish delegate from non-delegate
        case (_: Thicket) :: Import(_, expr, _) :: _ => buffer.addMemberCompletions(expr)
        case _                                       => buffer.addScopeCompletions
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

    private[this] val completions = new RenameAwareScope

    /**
     * Return the list of symbols that should be included in completion results.
     *
     * If several symbols share the same name, the type symbols appear before term symbols inside
     * the same `Completion`.
     */
    def getCompletions(implicit ctx: Context): List[Completion] = {
      val nameToSymbols = completions.mappings.toList
      nameToSymbols.map { case (name, symbols) =>
        val typesFirst = symbols.sortWith((s1, s2) => s1.isType && !s2.isType)
        val desc = description(typesFirst)
        Completion(name.show, desc, typesFirst)
      }
    }

    /**
     * A description for completion result that represents `symbols`.
     *
     * If `symbols` contains a single symbol, show its full name in case it's a type, or its type if
     * it's a term.
     *
     * When there are multiple symbols, show their kinds.
     */
    private def description(symbols: List[Symbol])(implicit ctx: Context): String = {
      symbols match {
        case sym :: Nil =>
          if (sym.isType) sym.showFullName
          else sym.info.widenTermRefExpr.show

        case sym :: _ =>
          symbols.map(ctx.printer.kindString).mkString("", " and ", s" ${sym.name.show}")

        case Nil =>
          ""
      }
    }

    /**
     * Add symbols that are currently in scope to `info`: the members of the current class and the
     * symbols that have been imported.
     */
    def addScopeCompletions(implicit ctx: Context): Unit = {
      if (ctx.owner.isClass) {
        addAccessibleMembers(ctx.owner.thisType)
        ctx.owner.asClass.classInfo.selfInfo match {
          case selfSym: Symbol => add(selfSym, selfSym.name)
          case _ =>
        }
      }
      else if (ctx.scope != null) ctx.scope.foreach(s => add(s, s.name))

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
      if (!qual.tpe.widenDealias.isBottomType) {
        addAccessibleMembers(qual.tpe)
        if (!mode.is(Mode.Import) && !qual.tpe.isRef(defn.NullClass)) {
          // Implicit conversions do not kick in when importing
          // and for `NullClass` they produce unapplicable completions (for unclear reasons)
          implicitConversionTargets(qual)(ctx.fresh.setExploreTyperState())
            .foreach(addAccessibleMembers)
        }
      }
    }

    /**
     * If `sym` exists, no symbol with the same name is already included, and it satisfies the
     * inclusion filter, then add it to the completions.
     */
    private def add(sym: Symbol, nameInScope: Name)(implicit ctx: Context) =
      if (sym.exists &&
          completionsFilter(NoType, nameInScope) &&
          !completions.lookup(nameInScope).exists &&
          include(sym, nameInScope)) {
        completions.enter(sym, nameInScope)
      }

    /** Lookup members `name` from `site`, and try to add them to the completion list. */
    private def addMember(site: Type, name: Name, nameInScope: Name)(implicit ctx: Context) =
      if (!completions.lookup(nameInScope).exists) {
        for (alt <- site.member(name).alternatives) add(alt.symbol, nameInScope)
      }

    /** Include in completion sets only symbols that
     *   1. start with given name prefix, and
     *   2. is not absent (info is not NoType)
     *   3. are not a primary constructor,
     *   4. have an existing source symbol,
     *   5. are the module class in case of packages,
     *   6. are mutable accessors, to exclude setters for `var`,
     *   7. symbol is not a package object
     *   8. symbol is not an artifact of the compiler
     *   9. have same term/type kind as name prefix given so far
     */
    private def include(sym: Symbol, nameInScope: Name)(implicit ctx: Context): Boolean =
      nameInScope.startsWith(prefix) &&
      !sym.isAbsent &&
      !sym.isPrimaryConstructor &&
      sym.sourceSymbol.exists &&
      (!sym.is(Package) || sym.is(ModuleClass)) &&
      !sym.is(allOf(Mutable, Accessor)) &&
      !sym.isPackageObject &&
      !sym.is(Artifact) &&
      (
           (mode.is(Mode.Term) && sym.isTerm)
        || (mode.is(Mode.Type) && (sym.isType || sym.isStableMember))
      )

    /**
     * Find all the members of `site` that are accessible and which should be included in `info`.
     *
     * @param site The type to inspect.
     * @return The members of `site` that are accessible and pass the include filter of `info`.
     */
    private def accessibleMembers(site: Type)(implicit ctx: Context): Seq[Symbol] = site match {
      case site: NamedType if site.symbol.is(Package) =>
        // Don't look inside package members -- it's too expensive.
        site.decls.toList.filter(sym => sym.isAccessibleFrom(site, superAccess = false))
      case _ =>
        def appendMemberSyms(name: Name, buf: mutable.Buffer[SingleDenotation]): Unit =
          try buf ++= site.member(name).alternatives
          catch { case ex: TypeError => }
        site.memberDenots(completionsFilter, appendMemberSyms).collect {
          case mbr if include(mbr.symbol, mbr.symbol.name) => mbr.accessibleFrom(site, superAccess = true).symbol
          case _ => NoSymbol
        }.filter(_.exists)
    }

    /** Add all the accessible members of `site` in `info`. */
    private def addAccessibleMembers(site: Type)(implicit ctx: Context): Unit =
      for (mbr <- accessibleMembers(site)) addMember(site, mbr.name, mbr.name)

    /**
     * Add in `info` the symbols that are imported by `ctx.importInfo`. If this is a wildcard import,
     * all the accessible members of the import's `site` are included.
     */
    private def addImportCompletions(implicit ctx: Context): Unit = {
      val imp = ctx.importInfo
      if (imp != null) {
        def addImport(name: TermName, nameInScope: TermName) = {
          addMember(imp.site, name, nameInScope)
          addMember(imp.site, name.toTypeName, nameInScope.toTypeName)
        }
        imp.reverseMapping.foreachBinding { (nameInScope, original) =>
          if (original != nameInScope || !imp.excluded.contains(original)) {
            addImport(original, nameInScope)
          }
        }
        if (imp.isWildcardImport)
          for (mbr <- accessibleMembers(imp.site) if !imp.excluded.contains(mbr.name.toTermName))
            addMember(imp.site, mbr.name, mbr.name)
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
      val conversions = new typer.ImplicitSearch(defn.AnyType, qual, pos.span).allImplicits
      val targets = conversions.map(_.widen.finalResultType)
      interactiv.println(i"implicit conversion targets considered: ${targets.toList}%, %")
      targets
    }

    /** Filter for names that should appear when looking for completions. */
   private[this] object completionsFilter extends NameFilter {
     def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
       !name.isConstructorName && name.toTermName.info.kind == SimpleNameKind
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

   /** A scope that tracks renames of the entered symbols.
    *  Useful for providing completions for renamed symbols
    *  in the REPL and the IDE.
    */
   private class RenameAwareScope extends Scopes.MutableScope {
     private[this] val nameToSymbols: mutable.Map[TermName, List[Symbol]] = mutable.Map.empty

     /** Enter the symbol `sym` in this scope, recording a potential renaming. */
     def enter[T <: Symbol](sym: T, name: Name)(implicit ctx: Context): T = {
       val termName = name.stripModuleClassSuffix.toTermName
       nameToSymbols += termName -> (sym :: nameToSymbols.getOrElse(termName, Nil))
       newScopeEntry(name, sym)
       sym
     }

     /** Get the names that are known in this scope, along with the list of symbols they refer to. */
     def mappings: Map[TermName, List[Symbol]] = nameToSymbols.toMap
   }

}
