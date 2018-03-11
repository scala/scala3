package dotty.tools.dotc.core.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.config.Printers._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.tasty.{TastyPickler, TastyPrinter, TastyString}

import scala.quoted.Types._
import scala.quoted.Exprs._

import scala.reflect.ClassTag

object PickledQuotes {
  import tpd._

  /** Pickle the quote into strings */
  def pickleQuote(tree: Tree)(implicit ctx: Context): scala.runtime.quoted.Unpickler.Pickled = {
    if (ctx.reporter.hasErrors) Nil
    else {
      val encapsulated = encapsulateQuote(tree)
      val pickled = pickle(encapsulated)
      TastyString.pickle(pickled)
    }
  }

  /** Transform the expression into its fully spliced Tree */
  def quotedExprToTree(expr: quoted.Expr[_])(implicit ctx: Context): Tree = expr match {
    case expr: TastyExpr[_] => unpickleExpr(expr)
    case expr: ValueExpr[_] => Literal(Constant(expr.value))
    case expr: TreeExpr[Tree] @unchecked => expr.tree
    case expr: FunctionAppliedTo[_, _] =>
      functionAppliedTo(quotedExprToTree(expr.f), quotedExprToTree(expr.x))
  }

  /** Transform the expression into its fully spliced TypeTree */
  def quotedTypeToTree(expr: quoted.Type[_])(implicit ctx: Context): Tree = expr match {
    case expr: TastyType[_] => unpickleType(expr)
    case expr: TaggedType[_] => classTagToTypeTree(expr.ct)
    case expr: TreeType[Tree] @unchecked => expr.tree
  }

  /** Unpickle the tree contained in the TastyExpr */
  private def unpickleExpr(expr: TastyExpr[_])(implicit ctx: Context): Tree = {
    val tastyBytes = TastyString.unpickle(expr.tasty)
    val unpickled = unpickle(tastyBytes, expr.args)
    unpickled match {
      case PackageDef(_, (vdef: ValDef) :: Nil) => vdef.rhs
    }
  }

  /** Unpickle the tree contained in the TastyType */
  private def unpickleType(ttpe: TastyType[_])(implicit ctx: Context): Tree = {
    val tastyBytes = TastyString.unpickle(ttpe.tasty)
    val unpickled = unpickle(tastyBytes, ttpe.args)
    unpickled match {
      case PackageDef(_, (vdef: ValDef) :: Nil) =>
        vdef.rhs.asInstanceOf[TypeApply].args.head
    }
  }

  /** Encapsulate the tree in a top level `val` or `type`
   *    `<tree>` ==> `package _root_ { val $quote: Any = <tree> }`
   *    or
   *    `<type tree>` ==> `package _root_ { val $typeQuote: Any = null.asInstanceOf[<tree>] }`
   */
  private def encapsulateQuote(tree: Tree)(implicit ctx: Context): Tree = {
    val name = (if (tree.isTerm) "$quote" else "$typeQuote").toTermName
    val sym = ctx.newSymbol(ctx.owner, name, Synthetic, defn.AnyType, coord = tree.coord)
    val encoded =
      if (tree.isTerm) tree
      else Literal(Constant(null)).select(nme.asInstanceOf_).appliedToTypeTrees(tree :: Nil)
    val quoted = ValDef(sym, encoded).withPos(tree.pos)
    PackageDef(ref(defn.RootPackage).asInstanceOf[Ident], quoted :: Nil).withPos(tree.pos)
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
    // if (tree.pos.exists)
    //   new PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tree :: Nil)

    // other pickle sections go here.
    val pickled = pickler.assembleParts()

    if (pickling ne noPrinter) {
      println(i"**** pickled quote of \n${tree.show}")
      new TastyPrinter(pickled).printContents()
    }

    pickled
  }

  /** Unpickle TASTY bytes into it's tree */
  private def unpickle(bytes: Array[Byte], splices: Seq[Any])(implicit ctx: Context): Tree = {
    val unpickler = new TastyUnpickler(bytes, splices)
    unpickler.enter(roots = Set(defn.RootPackage))
    val tree = unpickler.tree
    if (pickling ne noPrinter) {
      println(i"**** unpickled quote for \n${tree.show}")
      new TastyPrinter(bytes).printContents()
    }
    tree
  }

  private def classTagToTypeTree(ct: ClassTag[_])(implicit ctx: Context): TypeTree = {
    val tpe = ct match {
      case ClassTag.Unit => defn.UnitType
      case ClassTag.Byte => defn.ByteType
      case ClassTag.Char => defn.CharType
      case ClassTag.Short => defn.ShortType
      case ClassTag.Int => defn.IntType
      case ClassTag.Long => defn.LongType
      case ClassTag.Float => defn.FloatType
      case ClassTag.Double => defn.FloatType
    }
    TypeTree(tpe)
  }

  private def functionAppliedTo(f: Tree, x: Tree)(implicit ctx: Context): Tree = {
    val x1 = SyntheticValDef(NameKinds.UniqueName.fresh("x".toTermName), x)
    def x1Ref() = ref(x1.symbol)
    def rec(f: Tree): Tree = f match {
      case closureDef(ddef) =>
        new TreeMap() {
          private val paramSym = ddef.vparamss.head.head.symbol
          override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
            case tree: Ident if tree.symbol == paramSym => x1Ref().withPos(tree.pos)
            case _ => super.transform(tree)
          }
        }.transform(ddef.rhs)
      case Block(stats, expr) =>
        val applied = rec(expr)
        if (stats.isEmpty) applied
        else Block(stats, applied)
      case Inlined(call, bindings, expansion) =>
        Inlined(call, bindings, rec(expansion))
      case _ =>
        f.select(nme.apply).appliedTo(x1Ref())
    }
    Block(x1 :: Nil, rec(f))
  }
}
