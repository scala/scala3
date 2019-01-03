package dotty.tools.dotc
package transform

import core._
import Decorators._, Flags._, Types._, Contexts._, Symbols._, Constants._
import ast.Trees._
import ast.{TreeTypeMap, untpd}
import util.Spans._
import util.SourcePosition
import tasty.TreePickler.Hole
import SymUtils._
import NameKinds._
import dotty.tools.dotc.ast.tpd
import typer.Implicits.SearchFailureType

import scala.collection.mutable
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.typer.Inliner
import dotty.tools.dotc.util.SourcePosition


/** Translates quoted terms and types to `unpickle` method calls.
 *  Checks that the phase consistency principle (PCP) holds.
 *
 *
 *  Transforms top level quote
 *   ```
 *   '{ ...
 *      val x1 = ???
 *      val x2 = ???
 *      ...
 *      ~{ ... '{ ... x1 ... x2 ...} ... }
 *      ...
 *    }
 *    ```
 *  to
 *    ```
 *     unpickle(
 *       [[ // PICKLED TASTY
 *         ...
 *         val x1 = ???
 *         val x2 = ???
 *         ...
 *         Hole(0 | x1, x2)
 *         ...
 *       ]],
 *       List(
 *         (args: Seq[Any]) => {
 *           val x1$1 = args(0).asInstanceOf[Expr[T]]
 *           val x2$1 = args(1).asInstanceOf[Expr[T]] // can be asInstanceOf[Type[T]]
 *           ...
 *           { ... '{ ... x1$1.unary_~ ... x2$1.unary_~ ...} ... }
 *         }
 *       )
 *     )
 *    ```
 *  and then performs the same transformation on `'{ ... x1$1.unary_~ ... x2$1.unary_~ ...}`.
 *
 *
 *  For macro definitions we assume that we have a single ~ directly as the RHS.
 *  The Splicer is used to check that the RHS will be interpretable (with the `Splicer`) once inlined.
 */
class Staging extends MacroTransformWithImplicits {
  import tpd._
  import Staging._

  /** Classloader used for loading macros */
  private[this] var myMacroClassLoader: java.lang.ClassLoader = _
  private def macroClassLoader(implicit ctx: Context): ClassLoader = {
    if (myMacroClassLoader == null) {
      val urls = ctx.settings.classpath.value.split(java.io.File.pathSeparatorChar).map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
      myMacroClassLoader = new java.net.URLClassLoader(urls, getClass.getClassLoader)
    }
    myMacroClassLoader
  }

  override def phaseName: String = Staging.name

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case tree: RefTree if !ctx.inInlineMethod =>
        assert(!tree.symbol.isQuote)
        // assert(!tree.symbol.isSplice) // TODO widen ~ type references at stage 0?
        assert(tree.symbol != defn.QuotedExpr_~)
      case tree: Select if tree.symbol == defn.QuotedExpr_~ =>
        assert(Splicer.canBeSpliced(tree.qualifier))
      case _ =>
    }
  }

  override def run(implicit ctx: Context): Unit =
    if (ctx.compilationUnit.needsStaging) super.run

  protected def newTransformer(implicit ctx: Context): Transformer =
    new Reifier(inQuote = false, null, 0, new LevelInfo, new Embedded, ctx)

  private class LevelInfo {
    /** A map from locally defined symbols to the staging levels of their definitions */
    val levelOf = new mutable.HashMap[Symbol, Int]

    /** Register a reference defined in a quote but used in another quote nested in a splice.
     *  Returns a version of the reference that needs to be used in its place.
     *     '{
     *        val x = ???
     *        { ... '{ ... x ... } ... }.unary_~
     *      }
     *  Eta expanding the `x` in `{ ... '{ ... x ... } ... }.unary_~` will return a `x$1.unary_~` for which the `x$1`
     *  be created by some outer reifier.
     *
     *  This transformation is only applied to definitions at staging level 1.
     *
     *  See `isCaptured`
     */
    val capturers = new mutable.HashMap[Symbol, Tree => Tree]
  }

  /** The main transformer class
   *  @param  inQuote    we are within a `'(...)` context that is not shadowed by a nested `~(...)`
   *  @param  outer      the next outer reifier, null is this is the topmost transformer
   *  @param  level      the current level, where quotes add one and splices subtract one level.
   *                     The initial level is 0, a level `l` where `l > 0` implies code has been quoted `l` times
   *                     and `l == -1` is code inside a top level splice (in an inline method).
   *  @param  levels     a stacked map from symbols to the levels in which they were defined
   *  @param  embedded   a list of embedded quotes (if `inSplice = true`) or splices (if `inQuote = true`
   *  @param  rctx       the contex in the destination lifted lambda
   */
  private class Reifier(inQuote: Boolean, val outer: Reifier, val level: Int, levels: LevelInfo,
                        val embedded: Embedded, val rctx: Context) extends ImplicitsTransformer {
    import levels._
    assert(level >= -1)

    /** A nested reifier for a quote (if `isQuote = true`) or a splice (if not) */
    def nested(isQuote: Boolean)(implicit ctx: Context): Reifier = {
      val nestedEmbedded = if (level > 1 || (level == 1 && isQuote)) embedded else new Embedded
      new Reifier(isQuote, this, if (isQuote) level + 1 else level - 1, levels, nestedEmbedded, ctx)
    }

    /** We are in a `~(...)` context that is not shadowed by a nested `'(...)` */
    def inSplice: Boolean = outer != null && !inQuote

    /** We are not in a `~(...)` or a `'(...)` */
    def isRoot: Boolean = outer == null

    /** A map from type ref T to expressions of type `quoted.Type[T]`".
     *  These will be turned into splices using `addTags` and represent type variables
     *  that can be possibly healed.
     */
    val importedTags = new mutable.LinkedHashMap[TypeRef, Tree]()

    /** A map from type ref T to expressions of type `quoted.Type[T]`" like `importedTags`
      * These will be turned into splices using `addTags` and represent types spliced
      * explicitly.
      */
    val explicitTags = new mutable.LinkedHashSet[TypeRef]()

    /** A stack of entered symbols, to be unwound after scope exit */
    var enteredSyms: List[Symbol] = Nil

    /** Assuming importedTags = `Type1 -> tag1, ..., TypeN -> tagN`, the expression
     *
     *      { type <Type1> = <tag1>.unary_~
     *        ...
     *        type <TypeN> = <tagN>.unary_~
     *        <expr>
     *      }
     *
     *  references to `TypeI` in `expr` are rewired to point to the locally
     *  defined versions. As a side effect, prepend the expressions `tag1, ..., `tagN`
     *  as splices to `embedded`.
     */
    private def addTags(expr: Tree)(implicit ctx: Context): Tree = {

      def mkTagSymbolAndAssignType(typeRef: TypeRef, tag: Tree): Tree = {
        val rhs = transform(tag.select(tpnme.UNARY_~))
        val alias = ctx.typeAssigner.assignType(untpd.TypeBoundsTree(rhs, rhs), rhs, rhs)

        val original = typeRef.symbol.asType

        val local = ctx.newSymbol(
          owner = ctx.owner,
          name = UniqueName.fresh("T".toTermName).toTypeName,
          flags = Synthetic,
          info = TypeAlias(tag.tpe.select(tpnme.UNARY_~)),
          coord = typeRef.prefix.termSymbol.coord).asType

        ctx.typeAssigner.assignType(untpd.TypeDef(local.name, alias), local)
      }

      if (importedTags.isEmpty && explicitTags.isEmpty) expr
      else {
        val itags = importedTags.toList
        // The tree of the tag for each tag comes from implicit search in `tryHeal`
        val typeDefs = for ((tref, tag) <- itags) yield {
          mkTagSymbolAndAssignType(tref, tag)
        }
        importedTags.clear()

        // The tree of the tag for each tag comes from a type ref e.g., ~t
        val explicitTypeDefs = for (tref <- explicitTags) yield {
          val tag = ref(tref.prefix.termSymbol)
          mkTagSymbolAndAssignType(tref, tag)
        }
        val tagsExplicitTypeDefsPairs = explicitTags.zip(explicitTypeDefs)
        explicitTags.clear()

        // Maps type splices to type references of tags e.g., ~t -> some type T$1
        val map: Map[Type, Type] = {
          tagsExplicitTypeDefsPairs.map(x => (x._1, x._2.symbol.typeRef)) ++
          (itags.map(_._1) zip typeDefs.map(_.symbol.typeRef))
        }.toMap
        val tMap = new TypeMap() {
          override def apply(tp: Type): Type = map.getOrElse(tp, mapOver(tp))
        }

        Block(typeDefs ++ explicitTypeDefs,
          new TreeTypeMap(
            typeMap = tMap,
            substFrom = itags.map(_._1.symbol),
            substTo = typeDefs.map(_.symbol)
          ).apply(expr))
      }
    }

    /** Enter staging level of symbol defined by `tree`, if applicable. */
    def markDef(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case tree: DefTree =>
        val sym = tree.symbol
        if ((sym.isClass || !sym.maybeOwner.isType) && !levelOf.contains(sym)) {
          levelOf(sym) = level
          enteredSyms = sym :: enteredSyms
        }
      case _ =>
    }

    /** Does the level of `sym` match the current level?
     *  An exception is made for inline vals in macros. These are also OK if their level
     *  is one higher than the current level, because on execution such values
     *  are constant expression trees and we can pull out the constant from the tree.
     */
    def levelOK(sym: Symbol)(implicit ctx: Context): Boolean = levelOf.get(sym) match {
      case Some(l) =>
        l == level ||
        level == -1 && (
          sym == defn.TastyReflection_macroContext ||
          // here we assume that Splicer.canBeSpliced was true before going to level -1,
          // this implies that all non-inline arguments are quoted and that the following two cases are checked
          // on inline parameters or type parameters.
          sym.is(Param) ||
          sym.isClass // reference to this in inline methods
        )
      case None =>
        !sym.is(Param) || levelOK(sym.owner)
    }

    /** Try to heal phase-inconsistent reference to type `T` using a local type definition.
     *  @return None      if successful
     *  @return Some(msg) if unsuccessful where `msg` is a potentially empty error message
     *                    to be added to the "inconsistent phase" message.
     */
    def tryHeal(tp: Type, pos: SourcePosition)(implicit ctx: Context): Option[String] = tp match {
      case tp: TypeRef =>
        if (level == -1) {
          assert(ctx.inInlineMethod)
          None
        } else {
          val reqType = defn.QuotedTypeType.appliedTo(tp)
          val tag = ctx.typer.inferImplicitArg(reqType, pos.pos)
          tag.tpe match {
            case fail: SearchFailureType =>
              Some(i"""
                      |
                      | The access would be accepted with the right type tag, but
                      | ${ctx.typer.missingArgMsg(tag, reqType, "")}""")
            case _ =>
              importedTags(tp) = nested(isQuote = false).transform(tag)
              None
          }
        }
      case _ =>
        Some("")
    }

    /** Check reference to `sym` for phase consistency, where `tp` is the underlying type
     *  by which we refer to `sym`.
     */
    def check(sym: Symbol, tp: Type, pos: SourcePosition)(implicit ctx: Context): Unit = {
      val isThis = tp.isInstanceOf[ThisType]
      def symStr =
        if (!isThis) sym.show
        else if (sym.is(ModuleClass)) sym.sourceModule.show
        else i"${sym.name}.this"
      if (!isThis && sym.maybeOwner.isType && !sym.is(Param))
        check(sym.owner, sym.owner.thisType, pos)
      else if (level == 1 && sym.isType && sym.is(Param) && sym.owner.isInlineMethod && !outer.isRoot)
        importedTags(sym.typeRef) = capturers(sym)(ref(sym))
      else if (sym.exists && !sym.isStaticOwner && !levelOK(sym))
        for (errMsg <- tryHeal(tp, pos))
          ctx.error(em"""access to $symStr from wrong staging level:
                        | - the definition is at level ${levelOf.getOrElse(sym, 0)},
                        | - but the access is at level $level.$errMsg""", pos)
    }

    /** Check all named types and this-types in a given type for phase consistency. */
    def checkType(pos: SourcePosition)(implicit ctx: Context): TypeAccumulator[Unit] = new TypeAccumulator[Unit] {
      def apply(acc: Unit, tp: Type): Unit = reporting.trace(i"check type level $tp at $level") {
        tp match {
          case tp: TypeRef if tp.symbol.isSplice =>
            if (inQuote) {
              explicitTags += tp
              outer.checkType(pos).foldOver(acc, tp)
            }
            else {
              if (tp.isTerm) ctx.error(i"splice outside quotes", pos)
              tp
            }
          case tp: NamedType =>
            check(tp.symbol, tp, tp.symbol.sourcePos)
            if (!tp.symbol.is(Param))
              foldOver(acc, tp)
          case tp: ThisType =>
            check(tp.cls, tp, tp.cls.sourcePos)
            foldOver(acc, tp)
          case _ =>
            foldOver(acc, tp)
        }
      }
    }

    /** If `tree` refers to a locally defined symbol (either directly, or in a pickled type),
     *  check that its staging level matches the current level. References to types
     *  that are phase-incorrect can still be healed as follows:
     *
     *  If `T` is a reference to a type at the wrong level, heal it by setting things up
     *  so that we later add a type definition
     *
     *     type T' = ~quoted.Type[T]
     *
     *  to the quoted text and rename T to T' in it. This is done later in `reify` via
     *  `addTags`. `checkLevel` itself only records what needs to be done in the
     *  `typeTagOfRef` field of the current `Splice` structure.
     */
    private def checkLevel(tree: Tree)(implicit ctx: Context): Tree = {
      tree match {
        case (_: Ident) | (_: This) =>
          check(tree.symbol, tree.tpe, tree.sourcePos)
        case (_: UnApply)  | (_: TypeTree) =>
          checkType(tree.sourcePos).apply((), tree.tpe)
        case Select(qual, OuterSelectName(_, levels)) =>
          checkType(tree.sourcePos).apply((), tree.tpe.widen)
        case _: Bind =>
          checkType(tree.sourcePos).apply((), tree.symbol.info)
        case _: Template =>
          checkType(tree.sourcePos).apply((), tree.symbol.owner.asClass.givenSelfType)
        case _ =>
      }
      tree
    }

    /** Split `body` into a core and a list of embedded splices.
     *  Then if inside a splice, make a hole from these parts.
     *  If outside a splice, generate a call tp `scala.quoted.Unpickler.unpickleType` or
     *  `scala.quoted.Unpickler.unpickleExpr` that matches `tpe` with
     *  core and splices as arguments.
     */
    private def quotation(body: Tree, quote: Tree)(implicit ctx: Context): Tree = {
      val isType = quote.symbol eq defn.QuotedType_apply
      if (body.symbol.isSplice) {
        // simplify `'(~x)` to `x` and then transform it
        val Select(splice, _) = body
        transform(splice)
      }
      else if (level > 0) {
        val body1 = nested(isQuote = true).transform(body)
        // Keep quotes as trees to reduce pickled size and have a Expr.show without pickled quotes
        if (isType) ref(defn.QuotedType_apply).appliedToType(body1.tpe.widen)
        else ref(defn.QuotedExpr_apply).appliedToType(body1.tpe.widen).appliedTo(body1)
      }
      else body match {
        case body: RefTree if isCaptured(body.symbol, level + 1) =>
          // Optimization: avoid the full conversion when capturing `x`
          // in '{ x } to '{ x$1.unary_~ } and go directly to `x$1`
          capturers(body.symbol)(body)
        case _=>
          val (body1, splices) = nested(isQuote = true).split(body)
          if (level == 0 && !ctx.inInlineMethod) {
            val body2 =
              if (body1.isType) body1
              else Inlined(Inliner.inlineCallTrace(ctx.owner, quote.pos), Nil, body1)
            pickledQuote(body2, splices, body.tpe, isType).withPosOf(quote)
          }
          else {
            // In top-level splice in an inline def. Keep the tree as it is, it will be transformed at inline site.
            body
          }
      }
    }

    private def pickledQuote(body: Tree, splices: List[Tree], originalTp: Type, isType: Boolean)(implicit ctx: Context) = {
      def pickleAsValue[T](value: T) =
        ref(defn.Unpickler_liftedExpr).appliedToType(originalTp.widen).appliedTo(Literal(Constant(value)))
      def pickleAsTasty() = {
        val meth =
          if (isType) ref(defn.Unpickler_unpickleType).appliedToType(originalTp)
          else ref(defn.Unpickler_unpickleExpr).appliedToType(originalTp.widen)
        meth.appliedTo(
          liftList(PickledQuotes.pickleQuote(body).map(x => Literal(Constant(x))), defn.StringType),
          liftList(splices, defn.AnyType))
      }
      if (splices.nonEmpty) pickleAsTasty()
      else if (isType) {
        def tag(tagName: String) = ref(defn.QuotedTypeModule).select(tagName.toTermName)
        if (body.symbol == defn.UnitClass) tag("UnitTag")
        else if (body.symbol == defn.BooleanClass) tag("BooleanTag")
        else if (body.symbol == defn.ByteClass) tag("ByteTag")
        else if (body.symbol == defn.CharClass) tag("CharTag")
        else if (body.symbol == defn.ShortClass) tag("ShortTag")
        else if (body.symbol == defn.IntClass) tag("IntTag")
        else if (body.symbol == defn.LongClass) tag("LongTag")
        else if (body.symbol == defn.FloatClass) tag("FloatTag")
        else if (body.symbol == defn.DoubleClass) tag("DoubleTag")
        else pickleAsTasty()
      }
      else Staging.toValue(body) match {
        case Some(value) => pickleAsValue(value)
        case _ => pickleAsTasty()
      }
    }

    /** If inside a quote, split the body of the splice into a core and a list of embedded quotes
     *  and make a hole from these parts. Otherwise issue an error, unless we
     *  are in the body of an inline method.
     */
    private def splice(splice: Select)(implicit ctx: Context): Tree = {
      if (level > 1) {
        val body1 = nested(isQuote = false).transform(splice.qualifier)
        body1.select(splice.name)
      }
      else if (level == 1) {
        val (body1, quotes) = nested(isQuote = false).split(splice.qualifier)
        val tpe = outer.embedded.getHoleType(splice)
        val hole = makeHole(body1, quotes, tpe).withPosOf(splice)
        // We do not place add the inline marker for trees that where lifted as they come from the same file as their
        // enclosing quote. Any intemediate splice will add it's own Inlined node and cancel it before splicig the lifted tree.
        // Note that lifted trees are not necessarily expressions and that Inlined nodes are expected to be expressions.
        // For example we can have a lifted tree containing the LHS of an assignment (see tests/run-with-compiler/quote-var.scala).
        if (splice.isType || outer.embedded.isLiftedSymbol(splice.qualifier.symbol)) hole
        else Inlined(EmptyTree, Nil, hole)
      }
      else if (enclosingInlineds.nonEmpty) { // level 0 in an inlined call
        val spliceCtx = ctx.outer // drop the last `inlineContext`
        val pos: SourcePosition = spliceCtx.source.atPos(enclosingInlineds.head.pos)
        val evaluatedSplice = Splicer.splice(splice.qualifier, pos, macroClassLoader)(spliceCtx).withPosOf(splice)
        if (ctx.reporter.hasErrors) splice else transform(evaluatedSplice)
      }
      else if (!ctx.owner.isInlineMethod) { // level 0 outside an inline method
        ctx.error(i"splice outside quotes or inline method", splice.sourcePos)
        splice
      }
      else if (Splicer.canBeSpliced(splice.qualifier)) { // level 0 inside an inline definition
        nested(isQuote = false).split(splice.qualifier) // Just check PCP
        splice
      }
      else { // level 0 inside an inline definition
        ctx.error(
          "Malformed macro call. The contents of the ~ must call a static method and arguments must be quoted or inline.".stripMargin,
          splice.sourcePos)
        splice
      }
    }

    /** Transforms the contents of a nested splice
     *  Assuming
     *     '{
     *        val x = ???
     *        val y = ???
     *        { ... '{ ... x .. y ... } ... }.unary_~
     *      }
     *  then the spliced subexpression
     *     { ... '{ ... x ... y ... } ... }
     *  will be transformed to
     *     (args: Seq[Any]) => {
     *       val x$1 = args(0).asInstanceOf[Expr[Any]] // or .asInstanceOf[Type[Any]]
     *       val y$1 = args(1).asInstanceOf[Expr[Any]] // or .asInstanceOf[Type[Any]]
     *       { ... '{ ... x$1.unary_~ ... y$1.unary_~ ... } ... }
     *     }
     *
     *  See: `capture`
     *
     *  At the same time register `embedded` trees `x` and `y` to place as arguments of the hole
     *  placed in the original code.
     *     '{
     *        val x = ???
     *        val y = ???
     *        Hole(0 | x, y)
     *      }
     */
    private def makeLambda(tree: Tree)(implicit ctx: Context): Tree = {
      def body(arg: Tree)(implicit ctx: Context): Tree = {
        var i = 0
        transformWithCapturer(tree)(
          (captured: mutable.Map[Symbol, Tree]) => {
            (tree: Tree) => {
              def newCapture = {
                val tpw = tree.tpe.widen match {
                  case tpw: MethodicType => tpw.toFunctionType()
                  case tpw => tpw
                }
                assert(tpw.isInstanceOf[ValueType])
                val argTpe =
                  if (tree.isType) defn.QuotedTypeType.appliedTo(tpw)
                  else defn.QuotedExprType.appliedTo(tpw)
                val selectArg = arg.select(nme.apply).appliedTo(Literal(Constant(i))).asInstance(argTpe)
                val capturedArg = SyntheticValDef(UniqueName.fresh(tree.symbol.name.toTermName).toTermName, selectArg)
                i += 1
                embedded.addTree(tree, capturedArg.symbol)
                captured.put(tree.symbol, capturedArg)
                capturedArg
              }
              ref(captured.getOrElseUpdate(tree.symbol, newCapture).symbol)
            }
          }
        )
      }
      /* Lambdas are generated outside the quote that is beeing reified (i.e. in outer.rctx.owner).
       * In case the case that level == -1 the code is not in a quote, it is in an inline method,
       * hence we should take that as owner directly.
       */
      val lambdaOwner = if (level == -1) ctx.owner else outer.rctx.owner

      val tpe = MethodType(defn.SeqType.appliedTo(defn.AnyType) :: Nil, tree.tpe.widen)
      val meth = ctx.newSymbol(lambdaOwner, UniqueName.fresh(nme.ANON_FUN), Synthetic | Method, tpe)
      Closure(meth, tss => body(tss.head.head)(ctx.withOwner(meth)).changeOwner(ctx.owner, meth))
    }

    private def transformWithCapturer(tree: Tree)(capturer: mutable.Map[Symbol, Tree] => Tree => Tree)(implicit ctx: Context): Tree = {
      val captured = mutable.LinkedHashMap.empty[Symbol, Tree]
      val captured2 = capturer(captured)

      outer.enteredSyms.foreach(sym => if (!sym.isInlineMethod) capturers.put(sym, captured2))

      val tree2 = transform(tree)
      capturers --= outer.enteredSyms

      seq(captured.result().valuesIterator.toList, tree2)
    }

    /** Returns true if this tree will be captured by `makeLambda`. Checks phase consistency and presence of capturer. */
    private def isCaptured(sym: Symbol, level: Int)(implicit ctx: Context): Boolean =
      level == 1 && levelOf.get(sym).contains(1) && capturers.contains(sym)

    /** Transform `tree` and return the resulting tree and all `embedded` quotes
     *  or splices as a pair, after performing the `addTags` transform.
     */
    private def split(tree: Tree)(implicit ctx: Context): (Tree, List[Tree]) = {
      val tree1 = if (inQuote) addTags(transform(tree)) else makeLambda(tree)
      (tree1, embedded.getTrees)
    }

    /** Register `body` as an `embedded` quote or splice
     *  and return a hole with `splices` as arguments and the given type `tpe`.
     */
    private def makeHole(body: Tree, splices: List[Tree], tpe: Type)(implicit ctx: Context): Hole = {
      val idx = embedded.addTree(body, NoSymbol)
      Hole(idx, splices).withType(tpe).asInstanceOf[Hole]
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      reporting.trace(i"reify $tree at $level", show = true) {
        def mapOverTree(lastEntered: List[Symbol]) =
          try super.transform(tree)
          finally
            while (enteredSyms ne lastEntered) {
              levelOf -= enteredSyms.head
              enteredSyms = enteredSyms.tail
            }
        tree match {
          case Quoted(quotedTree) =>
            quotation(quotedTree, tree)
          case tree: TypeTree if tree.tpe.typeSymbol.isSplice =>
            val splicedType = tree.tpe.stripTypeVar.asInstanceOf[TypeRef].prefix.termSymbol
            splice(ref(splicedType).select(tpnme.UNARY_~).withPosOf(tree))
          case tree: Select if tree.symbol.isSplice =>
            splice(tree)
          case tree: RefTree if tree.symbol.is(Inline) && tree.symbol.is(Param) =>
            tree
          case tree: RefTree if isCaptured(tree.symbol, level) =>
            val t = capturers(tree.symbol).apply(tree)
            splice(t.select(if (tree.isTerm) nme.UNARY_~ else tpnme.UNARY_~))
          case Block(stats, _) =>
            val last = enteredSyms
            stats.foreach(markDef)
            mapOverTree(last)
          case CaseDef(pat, guard, body) =>
            val last = enteredSyms
            // mark all bindings
            new TreeTraverser {
              def traverse(tree: Tree)(implicit ctx: Context): Unit = {
                markDef(tree)
                traverseChildren(tree)
              }
            }.traverse(pat)
            mapOverTree(last)
          case _: Import =>
            tree
          case tree: DefDef if tree.symbol.is(Macro) && level == 0 =>
            if (enclosingInlineds.nonEmpty)
              return EmptyTree // Already checked at definition site and already inlined
            markDef(tree)
            tree.rhs match {
              case InlineSplice(_) =>
                mapOverTree(enteredSyms) // Ignore output, only check PCP
                cpy.DefDef(tree)(rhs = defaultValue(tree.rhs.tpe))
              case _ =>
                ctx.error(
                  """Malformed macro.
                    |
                    |Expected the ~ to be at the top of the RHS:
                    |  inline def foo(inline x: X, ..., y: Y): Int = ~impl(x, ... '(y))
                    |
                    | * The contents of the splice must call a static method
                    | * All arguments must be quoted or inline
                  """.stripMargin, tree.rhs.sourcePos)
                EmptyTree
            }
          case _ =>
            markDef(tree)
            checkLevel(mapOverTree(enteredSyms))
        }
      }

    private def liftList(list: List[Tree], tpe: Type)(implicit ctx: Context): Tree = {
      list.foldRight[Tree](ref(defn.NilModule)) { (x, acc) =>
        acc.select("::".toTermName).appliedToType(tpe).appliedTo(x)
      }
    }

    /** InlineSplice is used to detect cases where the expansion
     *  consists of a (possibly multiple & nested) block or a sole expression.
     */
    object InlineSplice {
      def unapply(tree: Tree)(implicit ctx: Context): Option[Tree] = tree match {
        case Select(qual, _) if tree.symbol.isSplice && Splicer.canBeSpliced(qual) => Some(qual)
        case Block(List(stat), Literal(Constant(()))) => unapply(stat)
        case Block(Nil, expr) => unapply(expr)
        case Typed(expr, _) => unapply(expr)
        case _ => None
      }
    }
  }
}

object Staging {
  import tpd._

  val name: String = "staging"

  def toValue(tree: tpd.Tree): Option[Any] = tree match {
    case Literal(Constant(c)) => Some(c)
    case Block(Nil, e) => toValue(e)
    case Inlined(_, Nil, e) => toValue(e)
    case _ => None
  }

  class Embedded(trees: mutable.ListBuffer[tpd.Tree] = mutable.ListBuffer.empty, map: mutable.Map[Symbol, tpd.Tree] = mutable.Map.empty) {
    /** Adds the tree and returns it's index */
    def addTree(tree: tpd.Tree, liftedSym: Symbol): Int = {
      trees += tree
      if (liftedSym ne NoSymbol)
        map.put(liftedSym, tree)
      trees.length - 1
    }

    /** Type used for the hole that will replace this splice */
    def getHoleType(splice: tpd.Select)(implicit ctx: Context): Type = {
      // For most expressions the splice.tpe but there are some types that are lost by lifting
      // that can be recoverd from the original tree. Currently the cases are:
      //  * Method types: the splice represents a method reference
      map.get(splice.qualifier.symbol).map(_.tpe.widen).getOrElse(splice.tpe)
    }

    def isLiftedSymbol(sym: Symbol)(implicit ctx: Context): Boolean = map.contains(sym)

    /** Get the list of embedded trees */
    def getTrees: List[tpd.Tree] = trees.toList

    override def toString: String = s"Embedded($trees, $map)"

  }
}
