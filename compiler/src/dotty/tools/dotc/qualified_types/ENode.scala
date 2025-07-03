package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Names.Designator
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.StdNames.tpnme
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{
  CachedProxyType,
  ConstantType,
  MethodType,
  NamedType,
  NoPrefix,
  SingletonType,
  TermRef,
  ThisType,
  Type
}

enum ENode:
  import ENode.*

  case Atom(tp: SingletonType)
  case Constructor(constr: Symbol)(val fields: List[Symbol])
  case Select(qual: ENode, member: Symbol)
  case Apply(fn: ENode, args: List[ENode])
  case OpApply(fn: ENode.Op, args: List[ENode])
  case TypeApply(fn: ENode, args: List[Type])
  case Lambda(paramTps: List[Type], retTp: Type, body: ENode)

  override def toString(): String =
    this match
      case Atom(tp)             => typeToString(tp)
      case Constructor(constr)  => s"new ${designatorToString(constr.lastKnownDenotation.owner)}"
      case Select(qual, member) => s"$qual.${designatorToString(member)}"
      case Apply(fn, args)      => s"$fn(${args.mkString(", ")})"
      case OpApply(op, args)    => s"(${args.mkString(op.operatorString())})"
      case TypeApply(fn, args)  => s"$fn[${args.map(typeToString).mkString(", ")}]"
      case Lambda(paramTps, retTp, body) =>
        s"(${paramTps.map(typeToString).mkString(", ")}): ${{ typeToString(retTp) }} => $body"

object ENode:
  private def typeToString(tp: Type): String =
    tp match
      case tp: NamedType =>
        val prefixString = if isEmptyPrefix(tp.prefix) then "" else typeToString(tp.prefix) + "."
        prefixString + designatorToString(tp.designator)
      case tp: ConstantType =>
        tp.value.value.toString
      case tp: ThisType =>
        typeToString(tp.tref) + ".this"
      case _ =>
        tp.toString

  private def isEmptyPrefix(tp: Type): Boolean =
    tp match
      case tp: NoPrefix.type =>
        true
      case tp: ThisType =>
        tp.tref.designator match
          case d: Symbol => d.lastKnownDenotation.name.toTermName == nme.EMPTY_PACKAGE
          case _         => false
      case _ => false

  private def designatorToString(d: Designator): String =
    d match
      case d: Symbol => d.lastKnownDenotation.name.toString
      case _         => d.toString

  enum Op:
    case IntSum
    case IntProduct
    case LongSum
    case LongProduct
    case Equal
    case Not
    case And
    case Or
    case LessThan

    def operatorString(): String =
      this match
        case IntSum      => "+"
        case IntProduct  => "*"
        case LongSum     => "+"
        case LongProduct => "*"
        case Equal       => "=="
        case Not         => "!"
        case And         => "&&"
        case Or          => "||"
        case LessThan    => "<"

  /** Reference to the argument of an [[ENode.Lambda]].
   *
   *  @param index 
   *    Debruijn index of the argument, starting from 0
   *  @param underyling
   *    Underlying type of the argument
   */
  final case class ArgRefType(index: Int, underlying: Type) extends CachedProxyType, SingletonType:
    override def underlying(using Context): Type = underlying
    override def computeHash(bs: Binders): Int = doHash(bs, index, underlying)
