package dotty.tools.dotc
package transform

import core._
import Decorators._, Flags._, Types._, Contexts._, Symbols._, Constants._
import Flags._
import ast.Trees._
import util.Positions._
import StdNames._
import ast.untpd
import MegaPhase.MiniPhase
import NameKinds.OuterSelectName
import scala.collection.mutable

/** Translates quoted terms and types to `unpickle` method calls.
 *  Checks that the phase consistency principle (PCP) holds.
 */
class ReifyQuotes extends MacroTransform {
  import ast.tpd._

  override def phaseName: String = "reifyQuotes"

  override def run(implicit ctx: Context): Unit =
    if (ctx.compilationUnit.containsQuotes) super.run

  protected def newTransformer(implicit ctx: Context): Transformer = new Reifier

  /** Is symbol a splice operation? */
  def isSplice(sym: Symbol)(implicit ctx: Context) =
    sym == defn.QuotedExpr_~ || sym == defn.QuotedType_~

  /** Serialize `tree`. Embedded splices are represented as nodes of the form
   *
   *      Select(qual, sym)
   *
   *  where `sym` is either `defn.QuotedExpr_~` or `defn.QuotedType_~`. For any splice,
   *  the `qual` part should not be pickled, since it will be added separately later
   *  as a splice.
   */
  def pickleTree(tree: Tree, isType: Boolean)(implicit ctx: Context): String =
    tree.show // TODO: replace with TASTY

  private class Reifier extends Transformer {

    /** A class for collecting the splices of some quoted expression */
    private class Splices {

      /** A listbuffer collecting splices */
      val buf = new mutable.ListBuffer[Tree]

      /** A map from type ref T to "expression of type quoted.Type[T]".
       *  These will be turned into splices using `addTags`
       */
      val typeTagOfRef = new mutable.LinkedHashMap[TypeRef, Tree]()

      /** Assuming typeTagOfRef = `Type1 -> tag1, ..., TypeN -> tagN`, the expression
       *
       *      { type <Type1> = <tag1>.unary_~
       *        ...
       *        type <TypeN> = <tagN>.unary.~
       *        <expr>
       *      }
       *
       *  where all references to `TypeI` in `expr` are rewired to point to the locally
       *  defined versions. As a side effect, append the expressions `tag1, ..., `tagN`
       *  as splices to `buf`.
       */
      def addTags(expr: Tree)(implicit ctx: Context): Tree =
        if (typeTagOfRef.isEmpty) expr
        else {
          val assocs = typeTagOfRef.toList
          val typeDefs = for ((tp, tag) <- assocs) yield {
            val original = tp.symbol.asType
            val rhs = tag.select(tpnme.UNARY_~)
            val alias = ctx.typeAssigner.assignType(untpd.TypeBoundsTree(rhs, rhs), rhs, rhs)
            val local = original.copy(
              owner = ctx.owner,
              flags = Synthetic,
              info = TypeAlias(tag.tpe.select(tpnme.UNARY_~)))
            ctx.typeAssigner.assignType(untpd.TypeDef(original.name, alias), local)
          }
          val (trefs, tags) = assocs.unzip
          tags ++=: buf
          typeTagOfRef.clear()
          Block(typeDefs, expr.subst(trefs.map(_.symbol), typeDefs.map(_.symbol)))
        }
    }

    /** The current staging level */
    private var currentLevel = 0

    /** The splices encountered so far, indexed by staging level */
    private val splicesAtLevel = mutable.ArrayBuffer(new Splices)

    // Invariant: -1 <= currentLevel <= splicesAtLevel.length

    /** A map from locally defined symbol's to the staging levels of their definitions */
    private val levelOf = new mutable.HashMap[Symbol, Int]

    /** A stack of entered symbol's, to be unwound after block exit */
    private var enteredSyms: List[Symbol] = Nil

    /** Enter staging level of symbol defined by `tree`, if applicable. */
    def markDef(tree: Tree)(implicit ctx: Context) = tree match {
      case tree: DefTree if !levelOf.contains(tree.symbol) =>
        levelOf(tree.symbol) = currentLevel
        enteredSyms = tree.symbol :: enteredSyms
      case _ =>
    }

    /** If `tree` refers to a locally defined symbol (either directly, or in a pickled type),
     *  check that its staging level matches the current level. References to types
     *  that are phase-incorrect can still be healed as follows.
     *
     *  If `T` is a reference to a type at the wrong level, heal it by setting things up
     *  so that we later add a type definition
     *
     *     type T' = ~quoted.Type[T]
     *
     *  to the quoted text and rename T to T' in it. This is done later in `reify` via
     *  `Splice#addTags`. checkLevel itself only records what needs to be done in the
     *  `typeTagOfRef` field of the current `Splice` structure.
     */
    private def checkLevel(tree: Tree)(implicit ctx: Context): Tree = {

      /** Check reference to `sym` for phase consistency, where `tp` is the underlying type
       *  by which we refer to `sym`.
       */
      def check(sym: Symbol, tp: Type): Unit = {
        val isThis = tp.isInstanceOf[ThisType]
        def symStr =
          if (!isThis) sym.show
          else if (sym.is(ModuleClass)) sym.sourceModule.show
          else i"${sym.name}.this"
        if (!isThis && sym.maybeOwner.isType)
          check(sym.owner, sym.owner.thisType)
        else if (sym.exists && !sym.isStaticOwner &&
                 !ctx.owner.ownersIterator.exists(_.isInlineMethod) &&
                 levelOf.getOrElse(sym, currentLevel) != currentLevel)
          tp match {
            case tp: TypeRef =>
              // Legalize reference to phase-inconstent type
              splicesAtLevel(currentLevel).typeTagOfRef(tp) = {
                currentLevel -= 1
                val tag = New(defn.QuotedTypeType.appliedTo(tp), Nil)
                try transform(tag) finally currentLevel += 1
              }
            case _ =>
              ctx.error(em"""access to $symStr from wrong staging level:
                            | - the definition is at level ${levelOf(sym)},
                            | - but the access is at level $currentLevel.""", tree.pos)
          }
      }

      /** Check all named types and this types in a given type for phase consistency */
      object checkType extends TypeAccumulator[Unit] {
        /** Check that all NamedType and ThisType parts of `tp` are level-correct.
         *  If they are not, try to heal with a local binding to a typetag splice
         */
        def apply(tp: Type): Unit = apply((), tp)
        def apply(acc: Unit, tp: Type): Unit = reporting.trace(i"check type level $tp at $currentLevel") {
          tp match {
            case tp: NamedType if isSplice(tp.symbol) =>
              currentLevel -= 1
              try foldOver(acc, tp) finally currentLevel += 1
            case tp: NamedType =>
              check(tp.symbol, tp)
              foldOver(acc, tp)
            case tp: ThisType =>
              check(tp.cls, tp)
              foldOver(acc, tp)
            case _ =>
              foldOver(acc, tp)
          }
        }
      }

      tree match {
        case (_: Ident) | (_: This) =>
          check(tree.symbol, tree.tpe)
        case (_: UnApply)  | (_: TypeTree) =>
          checkType(tree.tpe)
        case Select(qual, OuterSelectName(_, levels)) =>
          checkType(tree.tpe.widen)
        case _: Bind =>
          checkType(tree.symbol.info)
        case _: Template =>
          checkType(tree.symbol.owner.asClass.givenSelfType)
        case _ =>
      }
      tree
    }

    /** Turn `body` of quote into a call of `scala.quoted.Unpickler.unpickleType` or
     *  `scala.quoted.Unpickler.unpickleExpr` depending onwhether `isType` is true or not.
     *  The arguments to the method are:
     *
     *    - the serialized `body`, as returned from `pickleTree`
     *    - all splices found in `body`
     */
    private def reify(body: Tree, isType: Boolean)(implicit ctx: Context) = {
      currentLevel += 1
      if (currentLevel == splicesAtLevel.length) splicesAtLevel += null
      val splices = new Splices
      val savedSplices = splicesAtLevel(currentLevel)
      splicesAtLevel(currentLevel) = splices
      try {
        val body1 = splices.addTags(transform(body))
        ref(if (isType) defn.Unpickler_unpickleType else defn.Unpickler_unpickleExpr)
          .appliedToType(if (isType) body1.tpe else body1.tpe.widen)
          .appliedTo(
            Literal(Constant(pickleTree(body1, isType))),
            SeqLiteral(splices.buf.toList, TypeTree(defn.QuotedType)))
      }
      finally {
        splicesAtLevel(currentLevel) = savedSplices
        currentLevel -= 1
      }
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      reporting.trace(i"reify $tree at $currentLevel", show = true) {
        tree match {
          case Apply(fn, arg :: Nil) if fn.symbol == defn.quoteMethod =>
            reify(arg, isType = false)
          case TypeApply(fn, arg :: Nil) if fn.symbol == defn.typeQuoteMethod =>
            reify(arg, isType = true)
          case tree @ Select(body, name) if isSplice(tree.symbol) =>
            currentLevel -= 1
            val body1 = try transform(body) finally currentLevel += 1
            if (currentLevel > 0) {
              splicesAtLevel(currentLevel).buf += body1
              tree
            }
            else {
              if (currentLevel < 0)
                ctx.error(i"splice ~ not allowed under toplevel splice", tree.pos)
              cpy.Select(tree)(body1, name)
            }
          case Block(stats, _) =>
            val last = enteredSyms
            stats.foreach(markDef)
            try super.transform(tree)
            finally
              while (enteredSyms ne last) {
                levelOf -= enteredSyms.head
                enteredSyms = enteredSyms.tail
              }
          case Inlined(call, bindings, expansion @ Select(body, name)) if isSplice(expansion.symbol) =>
            // To maintain phase consistency, convert inlined expressions of the form
            // `{ bindings; ~expansion }` to `~{ bindings; expansion }`
            cpy.Select(expansion)(cpy.Inlined(tree)(call, bindings, body), name)
          case _: Import =>
            tree
          case _ =>
            markDef(tree)
            checkLevel(super.transform(tree))
        }
      }
  }
}