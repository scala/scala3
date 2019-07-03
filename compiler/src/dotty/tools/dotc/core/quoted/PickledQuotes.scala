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

import scala.internal.quoted._
import scala.reflect.ClassTag

object PickledQuotes {
  import tpd._

  /** Pickle the tree of the quote into strings */
  def pickleQuote(tree: Tree)(implicit ctx: Context): scala.runtime.quoted.Unpickler.Pickled = {
    if (ctx.reporter.hasErrors) Nil
    else {
      assert(!tree.isInstanceOf[Hole]) // Should not be pickled as it represents `'{$x}` which should be optimized to `x`
      val pickled = pickle(tree)
      TastyString.pickle(pickled)
    }
  }

  /** Transform the expression into its fully spliced Tree */
  def quotedExprToTree[T](expr: quoted.Expr[T])(implicit ctx: Context): Tree = expr match {
    case expr: TastyExpr[_] =>
      val unpickled = unpickleExpr(expr)
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
    case expr: TastyTreeExpr[Tree] @unchecked => healOwner(expr.tree)
    case expr: FunctionAppliedTo[_] =>
      functionAppliedTo(quotedExprToTree(expr.f), expr.args.map(arg => quotedExprToTree(arg)).toList)
  }

  /** Transform the expression into its fully spliced TypeTree */
  def quotedTypeToTree(expr: quoted.Type[_])(implicit ctx: Context): Tree = expr match {
    case expr: TastyType[_] =>
      unpickleType(expr) match {
        case Block(aliases, tpt) =>
          // `@quoteTypeTag type` aliasses are not required after unpickling
          tpt
        case tpt => tpt
      }
    case expr: TaggedType[_] => classTagToTypeTree(expr.ct)
    case expr: TreeType[Tree] @unchecked => healOwner(expr.typeTree)
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
  private def unpickleExpr(expr: TastyExpr[_])(implicit ctx: Context): Tree = {
    val tastyBytes = TastyString.unpickle(expr.tasty)
    unpickle(tastyBytes, expr.args, isType = false)(ctx.addMode(Mode.ReadPositions))
  }

  /** Unpickle the tree contained in the TastyType */
  private def unpickleType(ttpe: TastyType[_])(implicit ctx: Context): Tree = {
    val tastyBytes = TastyString.unpickle(ttpe.tasty)
    unpickle(tastyBytes, ttpe.args, isType = true)(ctx.addMode(Mode.ReadPositions))
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

  private def classTagToTypeTree(ct: ClassTag[_])(implicit ctx: Context): TypeTree = {
    val tpe = ct match {
      case ClassTag.Unit => defn.UnitType
      case ClassTag.Boolean => defn.BooleanType
      case ClassTag.Byte => defn.ByteType
      case ClassTag.Char => defn.CharType
      case ClassTag.Short => defn.ShortType
      case ClassTag.Int => defn.IntType
      case ClassTag.Long => defn.LongType
      case ClassTag.Float => defn.FloatType
      case ClassTag.Double => defn.DoubleType
    }
    TypeTree(tpe)
  }

  private def functionAppliedTo(fn: Tree, args: List[Tree])(implicit ctx: Context): Tree = {
    val (argVals, argRefs) = args.map(arg => arg.tpe match {
      case tpe: SingletonType if isIdempotentExpr(arg) => (Nil, arg)
      case _ =>
        val argVal = SyntheticValDef(NameKinds.UniqueName.fresh("x".toTermName), arg).withSpan(arg.span)
        (argVal :: Nil, ref(argVal.symbol))
    }).unzip
    def rec(fn: Tree): Tree = fn match {
      case Inlined(call, bindings, expansion) =>
        // this case must go before closureDef to avoid dropping the inline node
        cpy.Inlined(fn)(call, bindings, rec(expansion))
      case closureDef(ddef) =>
        val paramSyms = ddef.vparamss.head.map(param => param.symbol)
        val paramToVals = paramSyms.zip(argRefs).toMap
        new TreeTypeMap(
          oldOwners = ddef.symbol :: Nil,
          newOwners = ctx.owner :: Nil,
          treeMap = tree => paramToVals.get(tree.symbol).map(_.withSpan(tree.span)).getOrElse(tree)
        ).transform(ddef.rhs)
      case Block(stats, expr) =>
        seq(stats, rec(expr)).withSpan(fn.span)
      case _ =>
        fn.select(nme.apply).appliedToArgs(argRefs).withSpan(fn.span)
    }
    seq(argVals.flatten, rec(fn))
  }

  /** Make sure that the owner of this tree is `ctx.owner` */
  private def healOwner(tree: Tree)(implicit ctx: Context): Tree = {
    val getCurrentOwner = new TreeAccumulator[Option[Symbol]] {
      def apply(x: Option[Symbol], tree: tpd.Tree)(implicit ctx: Context): Option[Symbol] = {
        if (x.isDefined) x
        else tree match {
          case tree: DefTree => Some(tree.symbol.owner)
          case _ => foldOver(x, tree)
        }
      }
    }
    getCurrentOwner(None, tree) match {
      case Some(owner) if owner != ctx.owner => tree.changeOwner(owner, ctx.owner)
      case _ => tree
    }
  }
}
