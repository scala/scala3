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
import transform.CrossVersionChecks
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
      case fun @ Select(xt, op) if foldedBinops.contains(op) =>
        xt match
          case ConstantTree(x) =>
            tree.args match
              case ConstantTree(y) :: Nil =>
                CrossVersionChecks.checkDeprecatedDeferred(fun.symbol, fun.srcPos)
                tree.withFoldedType(foldBinop(op, x, y))
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

  private def foldUnop(op: Name, x: Constant): Constant | Null = (op, x.tag) match
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
  end foldUnop

  private def foldBinop(op: Name, x: Constant, y: Constant)(using Context): Constant | Null =
    val nme = StdNames.nme
    import nme.{Constant as _, *}
    inline def foldBooleanOp: Constant | Null = op match
      case ZOR  => Constant(x.booleanValue | y.booleanValue)
      case OR   => Constant(x.booleanValue | y.booleanValue)
      case XOR  => Constant(x.booleanValue ^ y.booleanValue)
      case ZAND => Constant(x.booleanValue & y.booleanValue)
      case AND  => Constant(x.booleanValue & y.booleanValue)
      case EQ   => Constant(x.booleanValue == y.booleanValue)
      case NE   => Constant(x.booleanValue != y.booleanValue)
      case _ => null
    inline def foldSubrangeOp: Constant | Null = op match
      case OR  => Constant(x.intValue | y.intValue)
      case XOR => Constant(x.intValue ^ y.intValue)
      case AND => Constant(x.intValue & y.intValue)
      case LSL => Constant(x.intValue << y.intValue)
      case LSR => Constant(x.intValue >>> y.intValue)
      case ASR => Constant(x.intValue >> y.intValue)
      case EQ  => Constant(x.intValue == y.intValue)
      case NE  => Constant(x.intValue != y.intValue)
      case LT  => Constant(x.intValue < y.intValue)
      case GT  => Constant(x.intValue > y.intValue)
      case LE  => Constant(x.intValue <= y.intValue)
      case GE  => Constant(x.intValue >= y.intValue)
      case ADD => Constant(x.intValue + y.intValue)
      case SUB => Constant(x.intValue - y.intValue)
      case MUL => Constant(x.intValue * y.intValue)
      case DIV => Constant(x.intValue / y.intValue)
      case MOD => Constant(x.intValue % y.intValue)
      case _ => null
    inline def foldLongOp: Constant | Null = op match
      case OR  => Constant(x.longValue | y.longValue)
      case XOR => Constant(x.longValue ^ y.longValue)
      case AND => Constant(x.longValue & y.longValue)
      case LSL => if (x.tag <= IntTag) Constant(x.intValue << y.longValue.toInt) else Constant(x.longValue << y.longValue)
      case LSR => if (x.tag <= IntTag) Constant(x.intValue >>> y.longValue.toInt) else Constant(x.longValue >>> y.longValue)
      case ASR => if (x.tag <= IntTag) Constant(x.intValue >> y.longValue.toInt) else Constant(x.longValue >> y.longValue)
      case EQ  => Constant(x.longValue == y.longValue)
      case NE  => Constant(x.longValue != y.longValue)
      case LT  => Constant(x.longValue < y.longValue)
      case GT  => Constant(x.longValue > y.longValue)
      case LE  => Constant(x.longValue <= y.longValue)
      case GE  => Constant(x.longValue >= y.longValue)
      case ADD => Constant(x.longValue + y.longValue)
      case SUB => Constant(x.longValue - y.longValue)
      case MUL => Constant(x.longValue * y.longValue)
      case DIV => Constant(x.longValue / y.longValue)
      case MOD => Constant(x.longValue % y.longValue)
      case _ => null
    inline def foldFloatOp: Constant | Null = op match
      case EQ  => Constant(x.floatValue == y.floatValue)
      case NE  => Constant(x.floatValue != y.floatValue)
      case LT  => Constant(x.floatValue < y.floatValue)
      case GT  => Constant(x.floatValue > y.floatValue)
      case LE  => Constant(x.floatValue <= y.floatValue)
      case GE  => Constant(x.floatValue >= y.floatValue)
      case ADD => Constant(x.floatValue + y.floatValue)
      case SUB => Constant(x.floatValue - y.floatValue)
      case MUL => Constant(x.floatValue * y.floatValue)
      case DIV => Constant(x.floatValue / y.floatValue)
      case MOD => Constant(x.floatValue % y.floatValue)
      case _ => null
    inline def foldDoubleOp: Constant | Null = op match
      case EQ  => Constant(x.doubleValue == y.doubleValue)
      case NE  => Constant(x.doubleValue != y.doubleValue)
      case LT  => Constant(x.doubleValue < y.doubleValue)
      case GT  => Constant(x.doubleValue > y.doubleValue)
      case LE  => Constant(x.doubleValue <= y.doubleValue)
      case GE  => Constant(x.doubleValue >= y.doubleValue)
      case ADD => Constant(x.doubleValue + y.doubleValue)
      case SUB => Constant(x.doubleValue - y.doubleValue)
      case MUL => Constant(x.doubleValue * y.doubleValue)
      case DIV => Constant(x.doubleValue / y.doubleValue)
      case MOD => Constant(x.doubleValue % y.doubleValue)
      case _ => null
    inline def foldStringOp: Constant | Null = op match
      case ADD => Constant(x.stringValue + y.stringValue)
      case EQ  => Constant(x.stringValue == y.stringValue)
      case NE  => Constant(x.stringValue != y.stringValue)
      case _ => null
    inline def foldNullOp: Constant | Null =
      assert(x.tag == NullTag || y.tag == NullTag)
      op match
        case EQ => Constant(x.tag == y.tag)
        case NE => Constant(x.tag != y.tag)
        case _ => null

    // begin foldBinop
    val optag =
      if (x.tag == y.tag) x.tag
      else if (x.isNumeric && y.isNumeric) math.max(x.tag, y.tag)
      else if (x.tag == NullTag || y.tag == NullTag) NullTag
      else NoTag

    try optag match
      case BooleanTag                            => foldBooleanOp
      case ByteTag | ShortTag | CharTag | IntTag => foldSubrangeOp
      case LongTag                               => foldLongOp
      case FloatTag                              => foldFloatOp
      case DoubleTag                             => foldDoubleOp
      case StringTag                             => foldStringOp
      case NullTag                               => foldNullOp
      case _                                     => null
    catch case ex: ArithmeticException => null // the code will crash at runtime,
                                               // but that is better than the
                                               // compiler itself crashing
  end foldBinop
end ConstFold
