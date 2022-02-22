package dotty.tools.dotc
package typer

import java.lang.ArithmeticException

import ast._
import core._
import Symbols._
import Types._
import Constants._
import Names._
import StdNames._
import Contexts._
import transform.TypeUtils._

object ConstFold:

  import tpd._

  private val foldedBinops = Set[Name](
    nme.ZOR, nme.OR, nme.XOR, nme.ZAND, nme.AND, nme.EQ, nme.NE,
    nme.LT, nme.GT, nme.LE, nme.GE, nme.LSL, nme.LSR, nme.ASR,
    nme.ADD, nme.SUB, nme.MUL, nme.DIV, nme.MOD)

  val foldedUnops = Set[Name](
    nme.UNARY_!, nme.UNARY_~, nme.UNARY_+, nme.UNARY_-)

  def Apply[T <: Apply](tree: T)(using Context): T =
    tree.fun match
      case Select(xt, op) if foldedBinops.contains(op) =>
        xt match
          case ConstantTree(x) =>
            tree.args match
              case yt :: Nil =>
                yt match
                  case ConstantTree(y) => tree.withFoldedType(foldBinop(op, x, y))
                  case _ => tree
              case _ => tree
          case _ => tree
      case TypeApply(Select(qual, nme.getClass_), _)
      if qual.tpe.widen.isPrimitiveValueType && tree.args.isEmpty =>
        tree.withFoldedType(Constant(qual.tpe.widen))
      case _ =>
        tree

  def Select[T <: Select](tree: T)(using Context): T =
    if foldedUnops.contains(tree.name) then
      tree.qualifier match
        case ConstantTree(x) => tree.withFoldedType(foldUnop(tree.name, x))
        case _ => tree
    else tree

  /** If tree is a constant operation, replace with result. */
  def apply[T <: Tree](tree: T)(using Context): T = tree match
    case tree: Apply => Apply(tree)
    case tree: Select => Select(tree)
    case TypeApply(_, targ :: Nil) if tree.symbol eq defn.Predef_classOf =>
      tree.withFoldedType(Constant(targ.tpe))
    case _ => tree

  private object ConstantTree:
    def unapply(tree: Tree)(using Context): Option[Constant] =
      tree match
        case Inlined(_, Nil, expr) => unapply(expr)
        case Typed(expr, _) => unapply(expr)
        case Literal(c) if c.tag == Constants.NullTag => Some(c)
        case _ =>
          tree.tpe.widenTermRefExpr.normalized.simplified match
            case ConstantType(c) => Some(c)
            case _ => None

  extension [T <: Tree](tree: T)(using Context)
    private def withFoldedType(c: Constant | Null): T =
      if c == null then tree else tree.withType(ConstantType(c)).asInstanceOf[T]

  private def foldUnop(op: Name, x: Constant): Constant = (op, x.tag) match {
    case (nme.UNARY_!, BooleanTag) => Constant(!x.booleanValue)

    case (nme.UNARY_~ , IntTag    ) => Constant(~x.intValue)
    case (nme.UNARY_~ , LongTag   ) => Constant(~x.longValue)

    case (nme.UNARY_+ , IntTag    ) => Constant(x.intValue)
    case (nme.UNARY_+ , LongTag   ) => Constant(x.longValue)
    case (nme.UNARY_+ , FloatTag  ) => Constant(x.floatValue)
    case (nme.UNARY_+ , DoubleTag ) => Constant(x.doubleValue)

    case (nme.UNARY_- , IntTag    ) => Constant(-x.intValue)
    case (nme.UNARY_- , LongTag   ) => Constant(-x.longValue)
    case (nme.UNARY_- , FloatTag  ) => Constant(-x.floatValue)
    case (nme.UNARY_- , DoubleTag ) => Constant(-x.doubleValue)

    case _ => null
  }

  /** These are local helpers to keep foldBinop from overly taxing the
   *  optimizer.
   */
  private def foldBooleanOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.ZOR  => Constant(x.booleanValue | y.booleanValue)
    case nme.OR   => Constant(x.booleanValue | y.booleanValue)
    case nme.XOR  => Constant(x.booleanValue ^ y.booleanValue)
    case nme.ZAND => Constant(x.booleanValue & y.booleanValue)
    case nme.AND  => Constant(x.booleanValue & y.booleanValue)
    case nme.EQ   => Constant(x.booleanValue == y.booleanValue)
    case nme.NE   => Constant(x.booleanValue != y.booleanValue)
    case _ => null
  }
  private def foldSubrangeOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.OR  => Constant(x.intValue | y.intValue)
    case nme.XOR => Constant(x.intValue ^ y.intValue)
    case nme.AND => Constant(x.intValue & y.intValue)
    case nme.LSL => Constant(x.intValue << y.intValue)
    case nme.LSR => Constant(x.intValue >>> y.intValue)
    case nme.ASR => Constant(x.intValue >> y.intValue)
    case nme.EQ  => Constant(x.intValue == y.intValue)
    case nme.NE  => Constant(x.intValue != y.intValue)
    case nme.LT  => Constant(x.intValue < y.intValue)
    case nme.GT  => Constant(x.intValue > y.intValue)
    case nme.LE  => Constant(x.intValue <= y.intValue)
    case nme.GE  => Constant(x.intValue >= y.intValue)
    case nme.ADD => Constant(x.intValue + y.intValue)
    case nme.SUB => Constant(x.intValue - y.intValue)
    case nme.MUL => Constant(x.intValue * y.intValue)
    case nme.DIV => Constant(x.intValue / y.intValue)
    case nme.MOD => Constant(x.intValue % y.intValue)
    case _ => null
  }
  private def foldLongOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.OR  => Constant(x.longValue | y.longValue)
    case nme.XOR => Constant(x.longValue ^ y.longValue)
    case nme.AND => Constant(x.longValue & y.longValue)
    case nme.LSL => if (x.tag <= IntTag) Constant(x.intValue << y.longValue.toInt) else Constant(x.longValue << y.longValue)
    case nme.LSR => if (x.tag <= IntTag) Constant(x.intValue >>> y.longValue.toInt) else Constant(x.longValue >>> y.longValue)
    case nme.ASR => if (x.tag <= IntTag) Constant(x.intValue >> y.longValue.toInt) else Constant(x.longValue >> y.longValue)
    case nme.EQ  => Constant(x.longValue == y.longValue)
    case nme.NE  => Constant(x.longValue != y.longValue)
    case nme.LT  => Constant(x.longValue < y.longValue)
    case nme.GT  => Constant(x.longValue > y.longValue)
    case nme.LE  => Constant(x.longValue <= y.longValue)
    case nme.GE  => Constant(x.longValue >= y.longValue)
    case nme.ADD => Constant(x.longValue + y.longValue)
    case nme.SUB => Constant(x.longValue - y.longValue)
    case nme.MUL => Constant(x.longValue * y.longValue)
    case nme.DIV => Constant(x.longValue / y.longValue)
    case nme.MOD => Constant(x.longValue % y.longValue)
    case _ => null
  }
  private def foldFloatOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.EQ  => Constant(x.floatValue == y.floatValue)
    case nme.NE  => Constant(x.floatValue != y.floatValue)
    case nme.LT  => Constant(x.floatValue < y.floatValue)
    case nme.GT  => Constant(x.floatValue > y.floatValue)
    case nme.LE  => Constant(x.floatValue <= y.floatValue)
    case nme.GE  => Constant(x.floatValue >= y.floatValue)
    case nme.ADD => Constant(x.floatValue + y.floatValue)
    case nme.SUB => Constant(x.floatValue - y.floatValue)
    case nme.MUL => Constant(x.floatValue * y.floatValue)
    case nme.DIV => Constant(x.floatValue / y.floatValue)
    case nme.MOD => Constant(x.floatValue % y.floatValue)
    case _ => null
  }
  private def foldDoubleOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.EQ  => Constant(x.doubleValue == y.doubleValue)
    case nme.NE  => Constant(x.doubleValue != y.doubleValue)
    case nme.LT  => Constant(x.doubleValue < y.doubleValue)
    case nme.GT  => Constant(x.doubleValue > y.doubleValue)
    case nme.LE  => Constant(x.doubleValue <= y.doubleValue)
    case nme.GE  => Constant(x.doubleValue >= y.doubleValue)
    case nme.ADD => Constant(x.doubleValue + y.doubleValue)
    case nme.SUB => Constant(x.doubleValue - y.doubleValue)
    case nme.MUL => Constant(x.doubleValue * y.doubleValue)
    case nme.DIV => Constant(x.doubleValue / y.doubleValue)
    case nme.MOD => Constant(x.doubleValue % y.doubleValue)
    case _ => null
  }
  private def foldStringOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.ADD => Constant(x.stringValue + y.stringValue)
    case nme.EQ  => Constant(x.stringValue == y.stringValue)
    case nme.NE  => Constant(x.stringValue != y.stringValue)
    case _ => null
  }

  private def foldNullOp(op: Name, x: Constant, y: Constant): Constant =
    assert(x.tag == NullTag || y.tag == NullTag)
    op match
      case nme.EQ => Constant(x.tag == y.tag)
      case nme.NE => Constant(x.tag != y.tag)
      case _ => null

  private def foldBinop(op: Name, x: Constant, y: Constant): Constant =
    val optag =
      if (x.tag == y.tag) x.tag
      else if (x.isNumeric && y.isNumeric) math.max(x.tag, y.tag)
      else if (x.tag == NullTag || y.tag == NullTag) NullTag
      else NoTag

    try optag match
      case  BooleanTag                            => foldBooleanOp(op, x, y)
      case  ByteTag | ShortTag | CharTag | IntTag => foldSubrangeOp(op, x, y)
      case  LongTag                               => foldLongOp(op, x, y)
      case  FloatTag                              => foldFloatOp(op, x, y)
      case  DoubleTag                             => foldDoubleOp(op, x, y)
      case  StringTag                             => foldStringOp(op, x, y)
      case  NullTag                               => foldNullOp(op, x, y)
      case  _                                     => null
      catch case ex: ArithmeticException => null // the code will crash at runtime,
                                                 // but that is better than the
                                                 // compiler itself crashing
  end foldBinop
end ConstFold
