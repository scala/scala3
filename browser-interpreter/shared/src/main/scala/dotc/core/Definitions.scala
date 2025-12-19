package dotc.core

import Names._
import Types._
import Flags._
import Symbols._
import Contexts._

/**
 * Cross-platform standard library definitions for the browser compiler.
 *
 * This initializes the basic types like Int, String, etc. that are
 * required for type checking.
 */
object Definitions {

  /** Initialize the standard library definitions */
  def init()(using ctx: Context): Unit = {
    // Create scala package
    val scalaPackage = newPackageSymbol(rootPackage, termName("scala"))
    rootPackage.enter(scalaPackage)

    // Create java.lang package
    val javaPackage = newPackageSymbol(rootPackage, termName("java"))
    rootPackage.enter(javaPackage)
    val langPackage = newPackageSymbol(javaPackage, termName("lang"))
    javaPackage.enter(langPackage)

    // Create primitive types
    defn.AnyClass = createClass(scalaPackage, "Any")
    defn.AnyType = defn.AnyClass.typeRef

    defn.AnyValType = createType(scalaPackage, "AnyVal", defn.AnyType)

    defn.AnyRefType = createType(scalaPackage, "AnyRef", defn.AnyType)
    defn.ObjectClass = createClass(langPackage, "Object", List(defn.AnyRefType))
    defn.ObjectType = defn.ObjectClass.typeRef

    defn.NothingClass = createClass(scalaPackage, "Nothing")
    defn.NothingType = defn.NothingClass.typeRef

    defn.NullClass = createClass(scalaPackage, "Null")
    defn.NullType = defn.NullClass.typeRef

    // Primitive value types
    defn.UnitClass = createClass(scalaPackage, "Unit", List(defn.AnyValType))
    defn.UnitType = defn.UnitClass.typeRef

    defn.BooleanClass = createClass(scalaPackage, "Boolean", List(defn.AnyValType))
    defn.BooleanType = defn.BooleanClass.typeRef

    defn.ByteClass = createClass(scalaPackage, "Byte", List(defn.AnyValType))
    defn.ByteType = defn.ByteClass.typeRef

    defn.ShortClass = createClass(scalaPackage, "Short", List(defn.AnyValType))
    defn.ShortType = defn.ShortClass.typeRef

    defn.CharClass = createClass(scalaPackage, "Char", List(defn.AnyValType))
    defn.CharType = defn.CharClass.typeRef

    defn.IntClass = createClass(scalaPackage, "Int", List(defn.AnyValType))
    defn.IntType = defn.IntClass.typeRef

    defn.LongClass = createClass(scalaPackage, "Long", List(defn.AnyValType))
    defn.LongType = defn.LongClass.typeRef

    defn.FloatClass = createClass(scalaPackage, "Float", List(defn.AnyValType))
    defn.FloatType = defn.FloatClass.typeRef

    defn.DoubleClass = createClass(scalaPackage, "Double", List(defn.AnyValType))
    defn.DoubleType = defn.DoubleClass.typeRef

    // String type
    defn.StringClass = createClass(langPackage, "String", List(defn.AnyRefType))
    defn.StringType = defn.StringClass.typeRef

    // Create common classes
    createClass(scalaPackage, "Array", List(defn.AnyRefType))
    createClass(scalaPackage, "List", List(defn.AnyRefType))
    createClass(scalaPackage, "Option", List(defn.AnyRefType))
    createClass(scalaPackage, "Some", List(defn.AnyRefType))
    createClass(scalaPackage, "None", List(defn.AnyRefType))
    createClass(scalaPackage, "Tuple1", List(defn.AnyRefType))
    createClass(scalaPackage, "Tuple2", List(defn.AnyRefType))
    createClass(scalaPackage, "Tuple3", List(defn.AnyRefType))

    // Create Function types
    for (arity <- 0 to 22) {
      createClass(scalaPackage, s"Function$arity", List(defn.AnyRefType))
    }

    // Create Predef
    val predefModule = newTermSymbol(scalaPackage, termName("Predef"), Module)
    predefModule.info = defn.ObjectType
    scalaPackage.enter(predefModule)
  }

  private def createClass(owner: PackageSymbol, name: String, parents: List[Type] = Nil): ClassSymbol = {
    val cls = newClassSymbol(owner, typeName(name))
    cls.parents = if (parents.isEmpty) List(defn.AnyType) else parents
    owner.enter(cls)
    cls
  }

  private def createType(owner: PackageSymbol, name: String, parent: Type): Type = {
    val cls = createClass(owner, name, List(parent))
    cls.typeRef
  }

  /** Check if a symbol is a primitive value class */
  def isPrimitiveValueClass(sym: Symbol): Boolean =
    sym == defn.BooleanClass ||
    sym == defn.ByteClass ||
    sym == defn.ShortClass ||
    sym == defn.CharClass ||
    sym == defn.IntClass ||
    sym == defn.LongClass ||
    sym == defn.FloatClass ||
    sym == defn.DoubleClass ||
    sym == defn.UnitClass

  /** Check if a type is a numeric type */
  def isNumericType(tp: Type): Boolean = tp.typeSymbol match {
    case s if s == defn.ByteClass => true
    case s if s == defn.ShortClass => true
    case s if s == defn.CharClass => true
    case s if s == defn.IntClass => true
    case s if s == defn.LongClass => true
    case s if s == defn.FloatClass => true
    case s if s == defn.DoubleClass => true
    case _ => false
  }

  /** Numeric widening order */
  def numericWidth(tp: Type): Int = tp.typeSymbol match {
    case s if s == defn.ByteClass => 1
    case s if s == defn.ShortClass => 2
    case s if s == defn.CharClass => 2
    case s if s == defn.IntClass => 3
    case s if s == defn.LongClass => 4
    case s if s == defn.FloatClass => 5
    case s if s == defn.DoubleClass => 6
    case _ => 0
  }
}

