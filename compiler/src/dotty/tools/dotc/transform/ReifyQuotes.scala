package dotty.tools.dotc
package transform

import core._
import Decorators._, Flags._, Types._, Contexts._, Symbols._, Constants._
import Flags._
import ast.Trees._
import ast.TreeTypeMap
import util.Positions._
import StdNames._
import ast.untpd
import tasty.TreePickler.Hole
import MegaPhase.MiniPhase
import SymUtils._
import NameKinds.OuterSelectName

import scala.collection.mutable
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.quoted.PickledQuotes


/** Translates quoted terms and types to `unpickle` method calls.
 *  Checks that the phase consistency principle (PCP) holds.
 */
class ReifyQuotes extends MacroTransform {
  import ast.tpd._

  override def phaseName: String = "reifyQuotes"

  override def run(implicit ctx: Context): Unit =
    if (ctx.compilationUnit.containsQuotesOrSplices) super.run

  protected def newTransformer(implicit ctx: Context): Transformer =
    new Reifier(inQuote = false, null, 0, new LevelInfo)

  private class LevelInfo {
    /** A map from locally defined symbols to the staging levels of their definitions */
    val levelOf = new mutable.HashMap[Symbol, Int]

    /** A stack of entered symbols, to be unwound after scope exit */
    var enteredSyms: List[Symbol] = Nil
  }

  /** A tree substituter that also works for holes */
  class SubstMap(
    typeMap: Type => Type = IdentityTypeMap,
    treeMap: Tree => Tree = identity _,
    oldOwners: List[Symbol] = Nil,
    newOwners: List[Symbol] = Nil,
    substFrom: List[Symbol],
    substTo: List[Symbol])(implicit ctx: Context)
  extends TreeTypeMap(typeMap, treeMap, oldOwners, newOwners, substFrom, substTo) {

    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case Hole(n, args) =>
        Hole(n, args.mapConserve(transform)).withPos(tree.pos).withType(mapType(tree.tpe))
      case _ =>
        super.transform(tree)
    }

    override def newMap(
        typeMap: Type => Type,
        treeMap: Tree => Tree,
        oldOwners: List[Symbol],
        newOwners: List[Symbol],
        substFrom: List[Symbol],
        substTo: List[Symbol])(implicit ctx: Context) =
      new SubstMap(typeMap, treeMap, oldOwners, newOwners, substFrom, substTo)
  }

  /** Requiring that `paramRefs` consists of a single reference `seq` to a Seq[Any],
   *  a tree map that replaces each hole with index `n` with `seq(n)`, applied
   *  to any arguments in the hole.
   */
  private def replaceHoles(paramRefs: List[Tree]) = new TreeMap {
    val seq :: Nil = paramRefs
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case Hole(n, args) =>
        val arg =
          seq.select(nme.apply).appliedTo(Literal(Constant(n))).ensureConforms(tree.tpe)
        if (args.isEmpty) arg
        else arg.select(nme.apply).appliedTo(SeqLiteral(args, TypeTree(defn.AnyType)))
      case _ =>
        super.transform(tree)
    }
  }

  /** If `tree` has holes, convert it to a function taking a `Seq` of elements as arguments
   *  where each hole is replaced by the corresponding sequence element.
   */
  private def elimHoles(tree: Tree)(implicit ctx: Context): Tree =
    if (tree.existsSubTree(_.isInstanceOf[Hole]))
      Lambda(
        MethodType(defn.SeqType.appliedTo(defn.AnyType) :: Nil, tree.tpe),
        replaceHoles(_).transform(tree))
    else tree

  /** The main transformer class
   *  @param  inQuote    we are within a `'(...)` context that is not shadowed by a nested `~(...)`
   *  @param  outer      the next outer reifier, null is this is the topmost transformer
   *  @param  level      the current level, where quotes add one and splices subtract one level
   *  @param  levels     a stacked map from symbols to the levels in which they were defined
   */
  private class Reifier(inQuote: Boolean, val outer: Reifier, val level: Int, levels: LevelInfo) extends Transformer {
    import levels._

    /** A nested reifier for a quote (if `isQuote = true`) or a splice (if not) */
    def nested(isQuote: Boolean): Reifier =
      new Reifier(isQuote, this, if (isQuote) level + 1 else level - 1, levels)

    /** We are in a `~(...)` context that is not shadowed by a nested `'(...)` */
    def inSplice = outer != null && !inQuote

    /** A list of embedded quotes (if `inSplice = true`) or splices (if `inQuote = true`) */
    val embedded = new mutable.ListBuffer[Tree]

    /** A map from type ref T to "expression of type `quoted.Type[T]`".
     *  These will be turned into splices using `addTags`
     */
    val importedTypes = new mutable.LinkedHashSet[TypeRef]()

    /** Assuming typeTagOfRef = `Type1 -> tag1, ..., TypeN -> tagN`, the expression
     *
     *      { type <Type1> = <tag1>.unary_~
     *        ...
     *        type <TypeN> = <tagN>.unary.~
     *        <expr>
     *      }
     *
     *  where all references to `TypeI` in `expr` are rewired to point to the locally
     *  defined versions. As a side effect, prepend the expressions `tag1, ..., `tagN`
     *  as splices to `buf`.
     */
    def addTags(expr: Tree)(implicit ctx: Context): Tree =
      if (importedTypes.isEmpty) expr
      else {
        val trefs = importedTypes.toList
        val typeDefs = for (tref <- trefs) yield {
          val tag = New(defn.QuotedTypeType.appliedTo(tref), Nil)
          val rhs = transform(tag.select(tpnme.UNARY_~))
          val alias = ctx.typeAssigner.assignType(untpd.TypeBoundsTree(rhs, rhs), rhs, rhs)
          val original = tref.symbol.asType
          val local = original.copy(
            owner = ctx.owner,
            flags = Synthetic,
            info = TypeAlias(tag.tpe.select(tpnme.UNARY_~)))
          ctx.typeAssigner.assignType(untpd.TypeDef(original.name, alias), local)
        }
        importedTypes.clear()
        Block(typeDefs,
          new SubstMap(substFrom = trefs.map(_.symbol), substTo = typeDefs.map(_.symbol))
            .apply(expr))
      }

    /** Enter staging level of symbol defined by `tree`, if applicable. */
    def markDef(tree: Tree)(implicit ctx: Context) = tree match {
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
        sym.is(Inline) && sym.owner.is(Macro) && sym.info.isValueType && l - 1 == level
      case None =>
        true
    }

    /** Issue a "splice outside quote" error unless we ar in the body of an inline method */
    def spliceOutsideQuotes(pos: Position)(implicit ctx: Context) =
      ctx.error(i"splice outside quotes", pos)

    /** Check reference to `sym` for phase consistency, where `tp` is the underlying type
     *  by which we refer to `sym`.
     */
    def check(sym: Symbol, tp: Type, pos: Position)(implicit ctx: Context): Unit = {
      val isThis = tp.isInstanceOf[ThisType]
      def symStr =
        if (!isThis) sym.show
        else if (sym.is(ModuleClass)) sym.sourceModule.show
        else i"${sym.name}.this"
      if (!isThis && sym.maybeOwner.isType)
        check(sym.owner, sym.owner.thisType, pos)
      else if (sym.exists && !sym.isStaticOwner && !levelOK(sym))
        tp match {
          case tp: TypeRef =>
            importedTypes += tp
          case _ =>
            ctx.error(em"""access to $symStr from wrong staging level:
                          | - the definition is at level ${levelOf(sym)},
                          | - but the access is at level $level.""", pos)
        }
    }

    /** Check all named types and this-types in a given type for phase consistency. */
    def checkType(pos: Position)(implicit ctx: Context): TypeAccumulator[Unit] = new TypeAccumulator[Unit] {
      def apply(acc: Unit, tp: Type): Unit = reporting.trace(i"check type level $tp at $level") {
        tp match {
          case tp: NamedType if tp.symbol.isSplice =>
            if (inQuote) outer.checkType(pos).foldOver(acc, tp)
            else {
              spliceOutsideQuotes(pos)
              tp
            }
          case tp: NamedType =>
            check(tp.symbol, tp, pos)
            foldOver(acc, tp)
          case tp: ThisType =>
            check(tp.cls, tp, pos)
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
          check(tree.symbol, tree.tpe, tree.pos)
        case (_: UnApply)  | (_: TypeTree) =>
          checkType(tree.pos).apply((), tree.tpe)
        case Select(qual, OuterSelectName(_, levels)) =>
          checkType(tree.pos).apply((), tree.tpe.widen)
        case _: Bind =>
          checkType(tree.pos).apply((), tree.symbol.info)
        case _: Template =>
          checkType(tree.pos).apply((), tree.symbol.owner.asClass.givenSelfType)
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
    private def quotation(body: Tree, quote: Tree)(implicit ctx: Context) = {
      val (body1, splices) = nested(isQuote = true).split(body)
      if (inSplice)
        makeHole(body1, splices, quote.tpe)
      else {
        val isType = quote.tpe.isRef(defn.QuotedTypeClass)
        ref(if (isType) defn.Unpickler_unpickleType else defn.Unpickler_unpickleExpr)
          .appliedToType(if (isType) body1.tpe else body1.tpe.widen)
          .appliedTo(
            Literal(Constant(PickledQuotes.pickleQuote(body1))),
            SeqLiteral(splices, TypeTree(defn.AnyType)))
      }
    }.withPos(quote.pos)

    /** If inside a quote, split `body` into a core and a list of embedded quotes
     *  and make a hole from these parts. Otherwise issue an error, unless we
     *  are in the body of an inline method.
     */
    private def splice(body: Tree, splice: Tree)(implicit ctx: Context): Tree = {
      if (inQuote) {
        val (body1, quotes) = nested(isQuote = false).split(body)
        makeHole(body1, quotes, splice.tpe)
      }
      else {
        spliceOutsideQuotes(splice.pos)
        splice
      }
    }.withPos(splice.pos)

    /** Transform `tree` and return the resulting tree and all `embedded` quotes
     *  or splices as a pair, after performing the `addTags` transform.
     */
    private def split(tree: Tree)(implicit ctx: Context): (Tree, List[Tree]) = {
      val tree1 = addTags(transform(tree))
      (tree1, embedded.toList.map(elimHoles))
    }

    /** Register `body` as an `embedded` quote or splice
     *  and return a hole with `splices` as arguments and the given type `tpe`.
     */
    private def makeHole(body: Tree, splices: List[Tree], tpe: Type)(implicit ctx: Context): Hole = {
      val idx = embedded.length
      embedded += body
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
          case Apply(fn, arg :: Nil) if fn.symbol == defn.quoteMethod =>
            quotation(arg, tree)
          case TypeApply(fn, arg :: Nil) if fn.symbol == defn.typeQuoteMethod =>
             quotation(arg, tree)
          case Select(body, _) if tree.symbol.isSplice =>
            splice(body, tree)
          case Block(stats, _) =>
            val last = enteredSyms
            stats.foreach(markDef)
            mapOverTree(last)
          case Inlined(call, bindings, expansion @ Select(body, name)) if expansion.symbol.isSplice =>
            // To maintain phase consistency, convert inlined expressions of the form
            // `{ bindings; ~expansion }` to `~{ bindings; expansion }`
            if (level == 0) transform(Splicer.splice(cpy.Inlined(tree)(call, bindings, body)))
            else transform(cpy.Select(expansion)(cpy.Inlined(tree)(call, bindings, body), name))
          case _: Import =>
            tree
          case tree: DefDef if tree.symbol.is(Macro) && level == 0 =>
            markDef(tree)
            val tree1 = nested(isQuote = true).transform(tree)
              // check macro code as it if appeared in a quoted context
            cpy.DefDef(tree)(rhs = EmptyTree)
          case _ =>
            markDef(tree)
            checkLevel(mapOverTree(enteredSyms))
        }
      }
  }
}