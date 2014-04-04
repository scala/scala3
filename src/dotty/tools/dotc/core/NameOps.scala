package dotty.tools.dotc
package core

import java.security.MessageDigest
import scala.annotation.switch
import scala.io.Codec
import Names._, StdNames._, Contexts._, Symbols._, Flags._
import Decorators.StringDecorator
import dotty.tools.dotc.util.Chars
import Chars.isOperatorPart

object NameOps {

  final object compactify {
    lazy val md5 = MessageDigest.getInstance("MD5")

    /** COMPACTIFY
     *
     *  The hashed name has the form (prefix + marker + md5 + marker + suffix), where
     *   - prefix/suffix.length = MaxNameLength / 4
     *   - md5.length = 32
     *
     *  We obtain the formula:
     *
     *   FileNameLength = 2*(MaxNameLength / 4) + 2.marker.length + 32 + 6
     *
     *  (+6 for ".class"). MaxNameLength can therefore be computed as follows:
     */
    def apply(s: String)(implicit ctx: Context): String = {
      val marker = "$$$$"
      val limit: Int = ctx.settings.maxClassfileName.value
      val MaxNameLength = (limit - 6) min 2 * (limit - 6 - 2 * marker.length - 32)

      def toMD5(s: String, edge: Int): String = {
        val prefix = s take edge
        val suffix = s takeRight edge

        val cs = s.toArray
        val bytes = Codec toUTF8 cs
        md5 update bytes
        val md5chars = (md5.digest() map (b => (b & 0xFF).toHexString)).mkString

        prefix + marker + md5chars + marker + suffix
      }

      if (s.length <= MaxNameLength) s else toMD5(s, MaxNameLength / 4)
    }
  }

  implicit class NameDecorator[N <: Name](val name: N) extends AnyVal {
    import nme._

    def likeTyped(n: Name): N =
      (if (name.isTermName) n.toTermName else n.toTypeName).asInstanceOf[N]

    def isConstructorName = name == CONSTRUCTOR || name == IMPLCLASS_CONSTRUCTOR
    def isExceptionResultName = name startsWith EXCEPTION_RESULT_PREFIX
    def isImplClassName = name endsWith IMPL_CLASS_SUFFIX
    def isLocalDummyName = name startsWith LOCALDUMMY_PREFIX
    def isLoopHeaderLabel = (name startsWith WHILE_PREFIX) || (name startsWith DO_WHILE_PREFIX)
    def isProtectedAccessorName = name startsWith PROTECTED_PREFIX
    def isSuperAccessorName = name startsWith SUPER_PREFIX
    def isReplWrapperName = name containsSlice INTERPRETER_IMPORT_WRAPPER
    def isSetterName = name endsWith SETTER_SUFFIX
    def isTraitSetterName = isSetterName && (name containsSlice TRAIT_SETTER_SEPARATOR)
    def isSingletonName = name endsWith SINGLETON_SUFFIX
    def isModuleClassName = name endsWith MODULE_SUFFIX
    def isImportName = name startsWith IMPORT
    def isInheritedName = name.head == '(' && name.startsWith(nme.INHERITED)

    def isModuleVarName(name: Name): Boolean =
      name.stripAnonNumberSuffix endsWith MODULE_VAR_SUFFIX

    /** Is name a variable name? */
    def isVariableName: Boolean = {
      val first = name.head
      (((first.isLower && first.isLetter) || first == '_')
        && (name != false_)
        && (name != true_)
        && (name != null_))
    }

    def isOpAssignmentName: Boolean = name match {
      case raw.NE | raw.LE | raw.GE | EMPTY =>
        false
      case _ =>
        name.last == '=' && name.head != '=' && isOperatorPart(name.head)
    }

    /** Is this the name of a higher-kinded type parameter? */
    def isHkParamName: Boolean = name(0) == '_' && name.startsWith(HK_PARAM_PREFIX)

    /** The index of the higher-kinded type parameter with this name.
     *  Pre: isHkParamName.
     */
    def hkParamIndex: Int = name.drop(name.lastIndexOf('$') + 1).toString.toInt

    /** If the name ends with $nn where nn are
      * all digits, strip the $ and the digits.
      * Otherwise return the argument.
      */
    def stripAnonNumberSuffix: Name = {
      var pos = name.length
      while (pos > 0 && name(pos - 1).isDigit)
      pos -= 1

      if (pos > 0 && pos < name.length && name(pos - 1) == '$')
        name take (pos - 1)
      else
        name
    }

    /** Convert this module name to corresponding module class name */
    def moduleClassName: TypeName = (name ++ tpnme.MODULE_SUFFIX).toTypeName

    /** Convert this module class name to corresponding source module name */
    def sourceModuleName: TermName = stripModuleClassSuffix.toTermName

    /** If name ends in module class suffix, drop it */
    def stripModuleClassSuffix: Name =
      if (isModuleClassName) name dropRight MODULE_SUFFIX.length else name

    /** If flags is a ModuleClass but not a Package, add module class suffix */
    def adjustIfModuleClass(flags: Flags.FlagSet): N =
      if (flags is (ModuleClass, butNot = Package)) name.asTypeName.moduleClassName.asInstanceOf[N]
      else name

    /** The expanded name of `name` relative to this class `base` with given `separator`
     */
    def expandedName(base: Symbol, separator: Name = nme.EXPAND_SEPARATOR)(implicit ctx: Context): N = {
      val prefix = if (base is Flags.ExpandedName) base.name else base.fullNameSeparated('$')
      name.fromName(prefix ++ separator ++ name).asInstanceOf[N]
    }

    def unexpandedName(separator: Name = nme.EXPAND_SEPARATOR): N = {
      val idx = name.lastIndexOfSlice(separator)
      if (idx < 0) name else (name drop (idx + separator.length)).asInstanceOf[N]
    }

    def inheritedName: N = likeTyped(nme.INHERITED ++ name)

    def revertInherited: N = likeTyped(name.drop(nme.INHERITED.length))

    /** Translate a name into a list of simple TypeNames and TermNames.
     *  In all segments before the last, type/term is determined by whether
     *  the following separator char is '.' or '#'.  The last segment
     *  is of the same type as the original name.
     *
     *  Examples:
     *
     *  package foo {
     *    object Lorax { object Wog ; class Wog }
     *    class Lorax  { object Zax ; class Zax }
     *  }
     *
     *  f("foo.Lorax".toTermName)  == List("foo": Term, "Lorax": Term) // object Lorax
     *  f("foo.Lorax".toTypeName)  == List("foo": Term, "Lorax": Type) // class Lorax
     *  f("Lorax.Wog".toTermName)  == List("Lorax": Term, "Wog": Term) // object Wog
     *  f("Lorax.Wog".toTypeName)  == List("Lorax": Term, "Wog": Type) // class Wog
     *  f("Lorax#Zax".toTermName)  == List("Lorax": Type, "Zax": Term) // object Zax
     *  f("Lorax#Zax".toTypeName)  == List("Lorax": Type, "Zax": Type) // class Zax
     *
     *  Note that in actual scala syntax you cannot refer to object Zax without an
     *  instance of Lorax, so Lorax#Zax could only mean the type.  One might think
     *  that Lorax#Zax.type would work, but this is not accepted by the parser.
     *  For the purposes of referencing that object, the syntax is allowed.
     */
    def segments: List[Name] = {
      def mkName(name: Name, follow: Char): Name =
        if (follow == '.') name.toTermName else name.toTypeName

      name.indexWhere(ch => ch == '.' || ch == '#') match {
        case -1 =>
          if (name.isEmpty) scala.Nil else name :: scala.Nil
        case idx =>
          mkName(name take idx, name(idx)) :: (name drop (idx + 1)).segments
      }
    }

    /** The variances of the higherKinded parameters of the trait named
     *  by this name.
     *  @pre The name is a higher-kinded trait name, i.e. it starts with HK_TRAIT_PREFIX
     */
    def hkVariances: List[Int] = {
      def varianceOfSuffix(suffix: Char): Int = {
        val idx = tpnme.varianceSuffixes.indexOf(suffix)
        assert(idx >= 0)
        idx - 1
      }
      name.drop(tpnme.HK_TRAIT_PREFIX.length).toList.map(varianceOfSuffix)
    }

    /** The name of the generic runtime operation corresponding to an array operation */
    def genericArrayOp: TermName = name match {
      case nme.apply => nme.array_apply
      case nme.length => nme.array_length
      case nme.update => nme.array_update
      case nme.clone_ => nme.array_clone
    }

    /** If name length exceeds allowable limit, replace part of it by hash */
    def compactified(implicit ctx: Context): TermName = termName(compactify(name.toString))
  }

  // needed???
  private val Boxed = Map[TypeName, TypeName](
    tpnme.Boolean -> jtpnme.BoxedBoolean,
    tpnme.Byte -> jtpnme.BoxedByte,
    tpnme.Char -> jtpnme.BoxedCharacter,
    tpnme.Short -> jtpnme.BoxedShort,
    tpnme.Int -> jtpnme.BoxedInteger,
    tpnme.Long -> jtpnme.BoxedLong,
    tpnme.Float -> jtpnme.BoxedFloat,
    tpnme.Double -> jtpnme.BoxedDouble)

  implicit class TermNameDecorator(val name: TermName) extends AnyVal {
    import nme._

    /** The expanded setter name of `name` relative to this class `base`
     */
    def expandedSetterName(base: Symbol)(implicit ctx: Context): TermName =
      name.expandedName(base, separator = TRAIT_SETTER_SEPARATOR)

    def setterName: TermName = name ++ SETTER_SUFFIX

    def setterToGetter: TermName = {
      val p = name.indexOfSlice(TRAIT_SETTER_SEPARATOR)
      if (p >= 0)
        (name drop (p + TRAIT_SETTER_SEPARATOR.length)).asTermName.setterToGetter
      else {
        assert(name.endsWith(SETTER_SUFFIX), name + " is referenced as a setter but has wrong name format")
        name.take(name.length - SETTER_SUFFIX.length).asTermName
      }
    }

    /** Nominally, name$default$N, encoded for <init>
     *  @param  Post the parameters position.
     *  @note Default getter name suffixes start at 1, so `pos` has to be adjusted by +1
     */
    def defaultGetterName(pos: Int): TermName = {
      val prefix = if (name.isConstructorName) DEFAULT_GETTER_INIT else name
      prefix ++ DEFAULT_GETTER ++ (pos + 1).toString
    }

    /** Nominally, name from name$default$N, CONSTRUCTOR for <init> */
    def defaultGetterToMethod: TermName = {
      val p = name.indexOfSlice(DEFAULT_GETTER)
      if (p >= 0) {
        val q = name.take(p).asTermName
        // i.e., if (q.decoded == CONSTRUCTOR.toString) CONSTRUCTOR else q
        if (q == DEFAULT_GETTER_INIT) CONSTRUCTOR else q
      } else name
    }

    /** If this is a default getter, its index (starting from 0), else -1 */
    def defaultGetterIndex: Int = {
      val p = name.indexOfSlice(DEFAULT_GETTER)
      if (p >= 0) name.drop(p + DEFAULT_GETTER.length).toString.toInt - 1
      else -1
    }

    /** The name of a super-accessor */
    def superAccessorName: TermName =
      SUPER_PREFIX ++ name

    /** The name of an accessor for protected symbols. */
    def protectedAccessorName: TermName =
      PROTECTED_PREFIX ++ name

    /** The name of a setter for protected symbols. Used for inherited Java fields. */
    def protectedSetterName(name: Name): TermName =
      PROTECTED_SET_PREFIX ++ name

    def moduleVarName: TermName =
      name ++ MODULE_VAR_SUFFIX

    /** The name unary_x for a prefix operator x */
    def toUnaryName: TermName = name match {
      case raw.MINUS => UNARY_-
      case raw.PLUS  => UNARY_+
      case raw.TILDE => UNARY_~
      case raw.BANG  => UNARY_!
      case _ => name
    }

    /** The name of a method which stands in for a primitive operation
     *  during structural type dispatch.
     */
    def primitiveInfixMethodName: TermName = name match {
      case OR   => takeOr
      case XOR  => takeXor
      case AND  => takeAnd
      case EQ   => testEqual
      case NE   => testNotEqual
      case ADD  => add
      case SUB  => subtract
      case MUL  => multiply
      case DIV  => divide
      case MOD  => takeModulo
      case LSL  => shiftSignedLeft
      case LSR  => shiftLogicalRight
      case ASR  => shiftSignedRight
      case LT   => testLessThan
      case LE   => testLessOrEqualThan
      case GE   => testGreaterOrEqualThan
      case GT   => testGreaterThan
      case ZOR  => takeConditionalOr
      case ZAND => takeConditionalAnd
      case _    => NO_NAME
    }

    /** Postfix/prefix, really.
     */
    def primitivePostfixMethodName: TermName = name match {
      case UNARY_!    => takeNot
      case UNARY_+    => positive
      case UNARY_-    => negate
      case UNARY_~    => complement
      case `toByte`   => toByte
      case `toShort`  => toShort
      case `toChar`   => toCharacter
      case `toInt`    => toInteger
      case `toLong`   => toLong
      case `toFloat`  => toFloat
      case `toDouble` => toDouble
      case _          => NO_NAME
    }

    def primitiveMethodName: TermName =
      primitiveInfixMethodName match {
        case NO_NAME => primitivePostfixMethodName
        case name => name
      }
    }
}