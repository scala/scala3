package dotc.core

import Names._
import Flags._

import scala.collection.mutable

/**
 * Cross-platform type representation for the browser compiler.
 *
 * This is a simplified type system for parsing and basic type checking.
 */
object Types {

  /** Base class for all types */
  abstract class Type {
    /** The underlying type (for refinements, annotations, etc.) */
    def underlying: Type = this

    /** Is this type a reference to a class? */
    def isRef(cls: Symbol): Boolean = false

    /** Type equality */
    def =:=(that: Type): Boolean = this == that

    /** Subtype check */
    def <:<(that: Type): Boolean = this =:= that

    /** The type symbol, if any */
    def typeSymbol: Symbol = NoSymbol

    /** The term symbol, if any */
    def termSymbol: Symbol = NoSymbol

    /** Member lookup */
    def member(name: Name): Denotation = NoDenotation

    /** Widen from singleton types */
    def widen: Type = this

    /** Dealias type aliases */
    def dealias: Type = this

    /** For debugging */
    def show: String = this.toString
  }

  /** No type (error) */
  case object NoType extends Type

  /** A type reference to a named type */
  case class TypeRef(prefix: Type, name: TypeName) extends Type {
    private var _symbol: Symbol = null
    def symbol: Symbol = _symbol
    def setSymbol(sym: Symbol): Unit = _symbol = sym

    override def typeSymbol: Symbol = symbol
    override def isRef(cls: Symbol): Boolean = symbol == cls
    override def toString: String = s"TypeRef($prefix, $name)"
  }

  /** A term reference to a named term */
  case class TermRef(prefix: Type, name: TermName) extends Type {
    private var _symbol: Symbol = null
    def symbol: Symbol = _symbol
    def setSymbol(sym: Symbol): Unit = _symbol = sym

    override def termSymbol: Symbol = symbol
    override def widen: Type = symbol.info
    override def toString: String = s"TermRef($prefix, $name)"
  }

  /** The type of `this` */
  case class ThisType(cls: Symbol) extends Type {
    override def toString: String = s"ThisType(${cls.name})"
  }

  /** The type of `super` */
  case class SuperType(thistpe: Type, supertpe: Type) extends Type

  /** A singleton type x.type */
  case class ConstantType(value: Constants.Constant) extends Type {
    override def widen: Type = value.tag match {
      case Constants.IntTag => defn.IntType
      case Constants.LongTag => defn.LongType
      case Constants.FloatTag => defn.FloatType
      case Constants.DoubleTag => defn.DoubleType
      case Constants.BooleanTag => defn.BooleanType
      case Constants.StringTag => defn.StringType
      case Constants.CharTag => defn.CharType
      case Constants.NullTag => defn.NullType
      case _ => NoType
    }
  }

  /** Applied type T[args] */
  case class AppliedType(tycon: Type, args: List[Type]) extends Type {
    override def typeSymbol: Symbol = tycon.typeSymbol
    override def toString: String = s"AppliedType($tycon, $args)"
  }

  /** Type bounds >: lo <: hi */
  case class TypeBounds(lo: Type, hi: Type) extends Type {
    def contains(tp: Type): Boolean = (lo <:< tp) && (tp <:< hi)
  }

  /** Method type (params): result */
  case class MethodType(paramNames: List[TermName], paramTypes: List[Type], resultType: Type) extends Type {
    override def toString: String = s"MethodType($paramNames, $paramTypes, $resultType)"
  }

  /** Polymorphic type [tparams]: result */
  case class PolyType(paramNames: List[TypeName], paramBounds: List[TypeBounds], resultType: Type) extends Type

  /** By-name type => T */
  case class ExprType(resultType: Type) extends Type {
    override def underlying: Type = resultType
  }

  /** Annotated type T @annot */
  case class AnnotatedType(parent: Type, annot: Any) extends Type {
    override def underlying: Type = parent
  }

  /** And type A & B */
  case class AndType(tp1: Type, tp2: Type) extends Type

  /** Or type A | B */
  case class OrType(tp1: Type, tp2: Type) extends Type

  /** A lazy type that will be completed later */
  abstract class LazyType extends Type {
    def complete(sym: Symbol): Unit
  }

  /** A refinement type { refinement } */
  case class RefinedType(parent: Type, refinedName: Name, refinedInfo: Type) extends Type {
    override def underlying: Type = parent
  }

  /** A recursive type */
  case class RecType(parent: Type) extends Type {
    override def underlying: Type = parent
  }

  /** A class info type */
  case class ClassInfo(
    prefix: Type,
    cls: Symbol,
    parents: List[Type],
    decls: Scope
  ) extends Type {
    override def typeSymbol: Symbol = cls
  }

  /** A package info type */
  case class PackageInfo(pkg: Symbol) extends Type

  // ============= Definitions placeholder =============

  /** Standard definitions - will be populated during initialization */
  object defn {
    var AnyType: Type = NoType
    var AnyValType: Type = NoType
    var AnyRefType: Type = NoType
    var NothingType: Type = NoType
    var NullType: Type = NoType
    var ObjectType: Type = NoType
    var IntType: Type = NoType
    var LongType: Type = NoType
    var FloatType: Type = NoType
    var DoubleType: Type = NoType
    var BooleanType: Type = NoType
    var CharType: Type = NoType
    var ByteType: Type = NoType
    var ShortType: Type = NoType
    var UnitType: Type = NoType
    var StringType: Type = NoType

    // Class symbols will be added during initialization
    var IntClass: Symbol = NoSymbol
    var LongClass: Symbol = NoSymbol
    var FloatClass: Symbol = NoSymbol
    var DoubleClass: Symbol = NoSymbol
    var BooleanClass: Symbol = NoSymbol
    var CharClass: Symbol = NoSymbol
    var ByteClass: Symbol = NoSymbol
    var ShortClass: Symbol = NoSymbol
    var UnitClass: Symbol = NoSymbol
    var StringClass: Symbol = NoSymbol
    var AnyClass: Symbol = NoSymbol
    var NothingClass: Symbol = NoSymbol
    var NullClass: Symbol = NoSymbol
    var ObjectClass: Symbol = NoSymbol

    def ClassType(tp: Type): Type = AppliedType(TypeRef(NoType, typeName("Class")), List(tp))
    def ArrayOf(elemTp: Type): Type = AppliedType(TypeRef(NoType, typeName("Array")), List(elemTp))

    def isFunctionClass(cls: Symbol): Boolean =
      cls.name.toString.startsWith("Function")
  }
}

/** A denotation is a binding of a name to a symbol/type */
sealed trait Denotation {
  def exists: Boolean
  def symbol: Symbol
  def info: Types.Type
  def name: Name
}

case object NoDenotation extends Denotation {
  def exists: Boolean = false
  def symbol: Symbol = NoSymbol
  def info: Types.Type = Types.NoType
  def name: Name = EmptyTermName
}

case class SingleDenotation(symbol: Symbol, info: Types.Type) extends Denotation {
  def exists: Boolean = true
  def name: Name = symbol.name
}

/** A scope containing symbol definitions */
class Scope {
  private val entries = mutable.LinkedHashMap[Name, Symbol]()

  def enter(sym: Symbol): Unit = entries(sym.name) = sym
  def lookup(name: Name): Symbol = entries.getOrElse(name, NoSymbol)
  def lookupEntry(name: Name): Denotation =
    entries.get(name).map(s => SingleDenotation(s, s.info)).getOrElse(NoDenotation)
  def iterator: Iterator[Symbol] = entries.valuesIterator
  def toList: List[Symbol] = entries.values.toList
  def isEmpty: Boolean = entries.isEmpty
  def size: Int = entries.size
}

object Scope {
  def empty: Scope = new Scope
}

