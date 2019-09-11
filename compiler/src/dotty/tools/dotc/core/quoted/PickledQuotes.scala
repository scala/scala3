package dotty.tools.dotc.core.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.config.Printers._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.tasty.TreePickler.Hole
import dotty.tools.dotc.core.tasty.{PositionPickler, TastyPickler, TastyPrinter, TastyString}
import dotty.tools.dotc.core.tasty.TreeUnpickler.UnpickleMode
import dotty.tools.dotc.quoted.QuoteContext
import dotty.tools.dotc.tastyreflect.ReflectionImpl

import scala.internal.quoted._
import scala.reflect.ClassTag

import scala.runtime.quoted.Unpickler._

object PickledQuotes {
  import tpd._

  /** Pickle the tree of the quote into strings */
  def pickleQuote(tree: Tree)(implicit ctx: Context): PickledQuote =
    if (ctx.reporter.hasErrors) Nil
    else {
      assert(!tree.isInstanceOf[Hole]) // Should not be pickled as it represents `'{$x}` which should be optimized to `x`
      val pickled = pickle(tree)
      TastyString.pickle(pickled)
    }

  /** Transform the expression into its fully spliced Tree */
  def quotedExprToTree[T](expr: quoted.Expr[T])(implicit ctx: Context): Tree = {
    val expr1 = expr.asInstanceOf[TastyTreeExpr[Tree]]
    QuoteContext.checkScopeId(expr1.scopeId)
    healOwner(expr1.tree)
  }

  /** Transform the expression into its fully spliced TypeTree */
  def quotedTypeToTree(tpe: quoted.Type[?])(implicit ctx: Context): Tree = {
    val tpe1 = tpe.asInstanceOf[TreeType[Tree]]
    QuoteContext.checkScopeId(tpe1.scopeId)
    healOwner(tpe1.typeTree)
  }

  private def dealiasTypeTags(tp: Type)(implicit ctx: Context): Type = new TypeMap() {
    override def apply(tp: Type): Type = {
      val tp1 = tp match {
        case tp: TypeRef if tp.typeSymbol.hasAnnotation(defn.InternalQuoted_QuoteTypeTagAnnot) => tp.dealias
        case _ => tp
      }
      mapOver(tp1)
    }
  }.apply(tp)

  /** Unpickle the tree contained in the TastyExpr */
  def unpickleExpr(tasty: PickledQuote, args: PickledExprArgs)(implicit ctx: Context): Tree = {
    val tastyBytes = TastyString.unpickle(tasty)
    val unpickled = unpickle(tastyBytes, args, isType = false)(ctx.addMode(Mode.ReadPositions))
    /** Force unpickling of the tree, removes the spliced type `@quotedTypeTag type` definitions and dealiases references to `@quotedTypeTag type` */
    val forceAndCleanArtefacts = new TreeMap {
      override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
        case Block(stat :: rest, expr1) if stat.symbol.hasAnnotation(defn.InternalQuoted_QuoteTypeTagAnnot) =>
          assert(rest.forall { case tdef: TypeDef => tdef.symbol.hasAnnotation(defn.InternalQuoted_QuoteTypeTagAnnot) })
          transform(expr1)
        case tree => super.transform(tree).withType(dealiasTypeTags(tree.tpe))
      }
    }
    forceAndCleanArtefacts.transform(unpickled)
  }

  /** Unpickle the tree contained in the TastyType */
  def unpickleType(tasty: PickledQuote, args: PickledTypeArgs)(implicit ctx: Context): Tree = {
    val tastyBytes = TastyString.unpickle(tasty)
    val unpickled = unpickle(tastyBytes, args, isType = true)(ctx.addMode(Mode.ReadPositions))
    val tpt = unpickled match {
      case Block(aliases, tpt) =>
        // `@quoteTypeTag type` aliases are not required after unpickling.
        // Type definitions are placeholders for type holes in the pickled quote, at this point
        // those holes have been filled. As we already dealias al references to them in `dealiasTypeTags`
        // there is no need to keep their definitions in the tree. As artifacts of quote reification
        // they also do not have a meaningful position in the source.
        val aliases1 = aliases.filter(!_.symbol.hasAnnotation(defn.InternalQuoted_QuoteTypeTagAnnot))
        seq(aliases1, tpt)
      case tpt => tpt
    }
    tpt.withType(dealiasTypeTags(tpt.tpe))
  }

  // TASTY picklingtests/pos/quoteTest.scala

  /** Pickle tree into it's TASTY bytes s*/
  private def pickle(tree: Tree)(implicit ctx: Context): Array[Byte] = {
    val pickler = new TastyPickler(defn.RootClass)
    val treePkl = pickler.treePkl
    treePkl.pickle(tree :: Nil)
    treePkl.compactify()
    pickler.addrOfTree = treePkl.buf.addrOfTree
    pickler.addrOfSym = treePkl.addrOfSym
    if (tree.span.exists)
      new PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tree :: Nil)

    if (quotePickling ne noPrinter)
      println(i"**** pickling quote of \n${tree.show}")

    val pickled = pickler.assembleParts()

    if (quotePickling ne noPrinter)
      println(new TastyPrinter(pickled).printContents())

    pickled
  }

  /** Unpickle TASTY bytes into it's tree */
  private def unpickle(bytes: Array[Byte], splices: Seq[Any], isType: Boolean)(implicit ctx: Context): Tree = {
    if (quotePickling ne noPrinter) {
      println(i"**** unpickling quote from TASTY")
      println(new TastyPrinter(bytes).printContents())
    }

    val mode = if (isType) UnpickleMode.TypeTree else UnpickleMode.Term
    val unpickler = new QuoteUnpickler(bytes, splices, mode)
    unpickler.enter(Set.empty)
    val tree = unpickler.tree

    if (quotePickling ne noPrinter)
      println(i"**** unpickle quote ${tree.show}")

    tree
  }

  /** Make sure that the owner of this tree is `ctx.owner` */
  private def healOwner(tree: Tree)(implicit ctx: Context): Tree = {
    val getCurrentOwner = new TreeAccumulator[Option[Symbol]] {
      def apply(x: Option[Symbol], tree: tpd.Tree)(implicit ctx: Context): Option[Symbol] =
        if (x.isDefined) x
        else tree match {
          case tree: DefTree => Some(tree.symbol.owner)
          case _ => foldOver(x, tree)
        }
    }
    getCurrentOwner(None, tree) match {
      case Some(owner) if owner != ctx.owner => tree.changeOwner(owner, ctx.owner)
      case _ => tree
    }
  }
}
