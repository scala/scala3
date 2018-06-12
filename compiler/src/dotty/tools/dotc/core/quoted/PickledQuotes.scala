package dotty.tools.dotc.core.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{TreeTypeMap, tpd}
import dotty.tools.dotc.config.Printers._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.tasty.TreePickler.Hole
import dotty.tools.dotc.core.tasty.{TastyPickler, TastyPrinter, TastyString}

import scala.quoted.Types._
import scala.quoted.Exprs._
import scala.reflect.ClassTag

object PickledQuotes {
  import tpd._

  /** Pickle the tree of the quoted.Expr */
  def pickleExpr(tree: Tree)(implicit ctx: Context): scala.quoted.Expr[Any] = {
    val pickled = pickleQuote(tree)
    scala.runtime.quoted.Unpickler.unpickleExpr(pickled, Nil)
  }

  /** Pickle the tree of the quote into strings */
  def pickleQuote(tree: Tree)(implicit ctx: Context): scala.runtime.quoted.Unpickler.Pickled = {
    if (ctx.reporter.hasErrors) Nil
    else {
      assert(!tree.isInstanceOf[Hole]) // Should not be pickled as it represents `'(~x)` which should be optimized to `x`
      val pickled = pickle(tree)
      TastyString.pickle(pickled)
    }
  }

  /** Transform the expression into its fully spliced Tree */
  def quotedExprToTree[T](expr: quoted.Expr[T])(implicit ctx: Context): Tree = expr match {
    case expr: TastyExpr[_] =>
      val unpickled = unpickleExpr(expr)
      val force = new TreeTraverser {
        def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = traverseChildren(tree)
      }
      force.traverse(unpickled)
      unpickled
    case expr: LiftedExpr[T] =>
      expr.value match {
        case value: Class[_] => ref(defn.Predef_classOf).appliedToType(classToType(value))
        case value => Literal(Constant(value))
      }
    case expr: TastyTreeExpr[Tree] @unchecked => expr.tree
    case expr: FunctionAppliedTo[_, _] =>
      functionAppliedTo(quotedExprToTree(expr.f), quotedExprToTree(expr.x))
  }

  /** Transform the expression into its fully spliced TypeTree */
  def quotedTypeToTree(expr: quoted.Type[_])(implicit ctx: Context): Tree = expr match {
    case expr: TastyType[_] => unpickleType(expr)
    case expr: TaggedType[_] => classTagToTypeTree(expr.ct)
    case expr: TreeType[Tree] @unchecked => expr.typeTree
  }

  /** Unpickle the tree contained in the TastyExpr */
  private def unpickleExpr(expr: TastyExpr[_])(implicit ctx: Context): Tree = {
    val tastyBytes = TastyString.unpickle(expr.tasty)
    unpickle(tastyBytes, expr.args, isType = false)
  }

  /** Unpickle the tree contained in the TastyType */
  private def unpickleType(ttpe: TastyType[_])(implicit ctx: Context): Tree = {
    val tastyBytes = TastyString.unpickle(ttpe.tasty)
    unpickle(tastyBytes, ttpe.args, isType = true)
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

    if (pickling ne noPrinter)
      println(i"**** pickling quote of \n${tree.show}")

    val pickled = pickler.assembleParts()

    if (pickling ne noPrinter)
      new TastyPrinter(pickled).printContents()

    pickled
  }

  /** Unpickle TASTY bytes into it's tree */
  private def unpickle(bytes: Array[Byte], splices: Seq[Any], isType: Boolean)(implicit ctx: Context): Tree = {
    if (pickling ne noPrinter) {
      println(i"**** unpickling quote from TASTY")
      new TastyPrinter(bytes).printContents()
    }

    val unpickler = new QuoteUnpickler(bytes, splices, isType)
    unpickler.enter(Set.empty)
    val tree = unpickler.tree

    if (pickling ne noPrinter)
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

  private def functionAppliedTo(f: Tree, x: Tree)(implicit ctx: Context): Tree = {
    val x1 = SyntheticValDef(NameKinds.UniqueName.fresh("x".toTermName), x)
    def x1Ref() = ref(x1.symbol)
    def rec(f: Tree): Tree = f match {
      case closureDef(ddef) =>
        val paramSym = ddef.vparamss.head.head.symbol
        new TreeTypeMap(
          oldOwners = ddef.symbol :: Nil,
          newOwners = ctx.owner :: Nil,
          treeMap = tree => if (tree.symbol == paramSym) x1Ref().withPos(tree.pos) else tree
        ).transform(ddef.rhs)
      case Block(stats, expr) =>
        seq(stats, rec(expr))
      case Inlined(call, bindings, expansion) =>
        Inlined(call, bindings, rec(expansion))
      case _ =>
        f.select(nme.apply).appliedTo(x1Ref())
    }
    Block(x1 :: Nil, rec(f))
  }

  private def classToType(clazz: Class[_])(implicit ctx: Context): Type = {
    if (clazz.isPrimitive) {
      if (clazz == classOf[Boolean]) defn.BooleanType
      else if (clazz == classOf[Byte]) defn.ByteType
      else if (clazz == classOf[Char]) defn.CharType
      else if (clazz == classOf[Short]) defn.ShortType
      else if (clazz == classOf[Int]) defn.IntType
      else if (clazz == classOf[Long]) defn.LongType
      else if (clazz == classOf[Float]) defn.FloatType
      else if (clazz == classOf[Double]) defn.DoubleType
      else defn.UnitType
    } else if (clazz.isArray) {
      defn.ArrayType.appliedTo(classToType(clazz.getComponentType))
    } else if (clazz.isMemberClass) {
      val name = clazz.getSimpleName.toTypeName
      val enclosing = classToType(clazz.getEnclosingClass)
      if (enclosing.member(name).exists) enclosing.select(name)
      else {
        enclosing.classSymbol.companionModule.termRef.select(name)
      }
    } else ctx.getClassIfDefined(clazz.getCanonicalName).typeRef
  }
}
