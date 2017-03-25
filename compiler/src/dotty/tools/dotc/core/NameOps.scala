package dotty.tools.dotc
package core

import java.security.MessageDigest
import scala.annotation.switch
import scala.io.Codec
import Names._, StdNames._, Contexts._, Symbols._, Flags._
import Decorators.PreNamedString
import util.{Chars, NameTransformer}
import Chars.isOperatorPart
import Definitions._
import config.Config

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

  class PrefixNameExtractor(pre: TermName, info: NameInfo) {
    def apply(name: TermName): TermName =
      if (Config.semanticNames) name.derived(info) else pre ++ name

    def unapply(name: TermName): Option[TermName] =
      if (Config.semanticNames)
        name match {
          case DerivedTermName(original, `info`) => Some(original)
          case _ => None
        }
      else tryUnmangle(name)

    def tryUnmangle(name: TermName): Option[TermName] =
      if (name startsWith pre) Some(name.drop(pre.length).asTermName)
      else None
  }

  object SuperAccessorName extends PrefixNameExtractor(nme.SUPER_PREFIX, NameInfo.SuperAccessor)
  object InitializerName extends PrefixNameExtractor(nme.INITIALIZER_PREFIX, NameInfo.Initializer)

  implicit class NameDecorator[N <: Name](val name: N) extends AnyVal {
    import nme._

    def likeTyped(n: PreName): N =
      (if (name.isTermName) n.toTermName else n.toTypeName).asInstanceOf[N]

    def isConstructorName = name == CONSTRUCTOR || name == TRAIT_CONSTRUCTOR
    def isStaticConstructorName = name == STATIC_CONSTRUCTOR
    def isImplClassName = name endsWith IMPL_CLASS_SUFFIX
    def isLocalDummyName = name startsWith LOCALDUMMY_PREFIX
    def isLoopHeaderLabel = (name startsWith WHILE_PREFIX) || (name startsWith DO_WHILE_PREFIX)
    def isProtectedAccessorName = name startsWith PROTECTED_PREFIX
    def isReplWrapperName = name.toSimpleName containsSlice INTERPRETER_IMPORT_WRAPPER
    def isTraitSetterName = name.toSimpleName containsSlice TRAIT_SETTER_SEPARATOR
    def isSetterName = name endsWith SETTER_SUFFIX
    def isSingletonName = name endsWith SINGLETON_SUFFIX
    def isModuleClassName =
      if (Config.semanticNames) name.is(NameInfo.ModuleClass.kind)
      else name endsWith MODULE_SUFFIX
    def isAvoidClashName = name endsWith AVOID_CLASH_SUFFIX
    def isImportName = name startsWith IMPORT
    def isFieldName = name endsWith LOCAL_SUFFIX
    def isShadowedName = name.startsWith(nme.SHADOWED)
    def isDefaultGetterName = name.isTermName && name.asTermName.defaultGetterIndex >= 0
    def isScala2LocalSuffix = name.endsWith(" ")
    def isModuleVarName(name: Name): Boolean =
      name.stripAnonNumberSuffix endsWith MODULE_VAR_SUFFIX
    def isSelectorName = name.startsWith(" ") && name.tail.forall(_.isDigit)
    def isLazyLocal = name.endsWith(nme.LAZY_LOCAL)
    def isOuterSelect = name.endsWith(nme.OUTER_SELECT)
    def isInlineAccessor = name.startsWith(nme.INLINE_ACCESSOR_PREFIX)

    /** Is name a variable name? */
    def isVariableName: Boolean = name.length > 0 && {
      val first = name.head
      (((first.isLower && first.isLetter) || first == '_')
        && (name != false_)
        && (name != true_)
        && (name != null_))
    }

    def isOpAssignmentName: Boolean = name match {
      case raw.NE | raw.LE | raw.GE | EMPTY =>
        false
      case name: SimpleTermName =>
        name.length > 0 && name.last == '=' && name.head != '=' && isOperatorPart(name.head)
      case _ =>
        false
    }

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
    def moduleClassName: TypeName =
      if (Config.semanticNames) name.derived(NameInfo.ModuleClass).toTypeName
      else (name ++ tpnme.MODULE_SUFFIX).toTypeName

    /** Convert this module class name to corresponding source module name */
    def sourceModuleName: TermName = stripModuleClassSuffix.toTermName

    /** If name ends in module class suffix, drop it */
    def stripModuleClassSuffix: Name =
      if (isModuleClassName)
        if (Config.semanticNames) name.exclude(NameInfo.ModuleClass.kind)
        else name dropRight MODULE_SUFFIX.length
      else name

    /** Append a suffix so that this name does not clash with another name in the same scope */
    def avoidClashName: TermName = (name ++ AVOID_CLASH_SUFFIX).toTermName

    /** If name ends in "avoid clash" suffix, drop it */
    def stripAvoidClashSuffix: Name =
      if (isAvoidClashName) name dropRight AVOID_CLASH_SUFFIX.length else name

    /** If flags is a ModuleClass but not a Package, add module class suffix */
    def adjustIfModuleClass(flags: Flags.FlagSet): N = {
      if (flags is (ModuleClass, butNot = Package)) name.asTypeName.moduleClassName
      else stripAvoidClashSuffix
    }.asInstanceOf[N]

    /** The superaccessor for method with given name */
    def superName: TermName =
      if (Config.semanticNames) name.derived(NameInfo.SuperAccessor).toTermName
      else (nme.SUPER_PREFIX ++ name).toTermName

    /** The expanded name of `name` relative to given class `base`.
     */
    def expandedName(base: Symbol, separator: Name)(implicit ctx: Context): N =
      expandedName(if (base is Flags.ExpandedName) base.name else base.fullNameSeparated("$"), separator)

    def expandedName(base: Symbol)(implicit ctx: Context): N = expandedName(base, nme.EXPAND_SEPARATOR)

    /** The expanded name of `name` relative to `basename` with given `separator`
     */
    def expandedName(prefix: Name, separator: Name = nme.EXPAND_SEPARATOR): N =
      likeTyped(
        if (Config.semanticNames) {
          def qualify(name: SimpleTermName) =
            prefix.derived(NameInfo.qualifier(separator.toString)(name))
          name rewrite {
            case name: SimpleTermName =>
              qualify(name)
            case DerivedTermName(_, _: NameInfo.Qualified) =>
              // Note: an expanded name may itself be expanded. For example, look at javap of scala.App.initCode
              qualify(name.toSimpleName)
          }
        }
        else prefix ++ separator ++ name)

    def expandedName(prefix: Name): N = expandedName(prefix, nme.EXPAND_SEPARATOR)

    /** Revert the expanded name.
     *  Note: This currently gives incorrect results
     *  if the normal name contains `nme.EXPAND_SEPARATOR`, i.e. two consecutive '$'
     *  signs. This can happen for instance if a super accessor is paired with
     *  an encoded name, e.g. super$$plus$eq. See #765.
     */
    def unexpandedName: N =
      if (Config.semanticNames)
        likeTyped {
          name.rewrite { case DerivedTermName(_, NameInfo.Expand(unexp)) => unexp }
        }
      else unexpandedNameOfMangled

    def unexpandedNameOfMangled: N = likeTyped {
      var idx = name.lastIndexOfSlice(nme.EXPAND_SEPARATOR)

      // Hack to make super accessors from traits work. They would otherwise fail because of #765
      // TODO: drop this once we have more robust name handling
      if (idx > FalseSuperLength && name.slice(idx - FalseSuperLength, idx) == FalseSuper)
        idx -= FalseSuper.length

      if (idx < 0) name else (name drop (idx + nme.EXPAND_SEPARATOR.length))
    }

    def expandedPrefix: N =
      if (Config.semanticNames)
        likeTyped {
          name.rewrite { case DerivedTermName(prefix, NameInfo.Expand(_)) => prefix }
        }
      else expandedPrefixOfMangled

    def expandedPrefixOfMangled: N = {
      val idx = name.lastIndexOfSlice(nme.EXPAND_SEPARATOR)
      assert(idx >= 0)
      likeTyped(name.take(idx))
    }

    def unmangleExpandedName: N =
      if (Config.semanticNames && name.isSimple) {
        val unmangled = unexpandedNameOfMangled
        if (name eq unmangled) name
        else likeTyped(expandedPrefixOfMangled.derived(NameInfo.Expand(unmangled.asSimpleName)))
      }
      else name

    def shadowedName: N = likeTyped(nme.SHADOWED ++ name)

    def revertShadowed: N = likeTyped(name.drop(nme.SHADOWED.length))

    def implClassName: N = likeTyped(name ++ tpnme.IMPL_CLASS_SUFFIX)

    def errorName: N = likeTyped(name ++ nme.ERROR)

    def directName: N = likeTyped(name ++ DIRECT_SUFFIX)

    def freshened(implicit ctx: Context): N =
      likeTyped(
        if (name.isModuleClassName) name.stripModuleClassSuffix.freshened.moduleClassName
        else likeTyped(ctx.freshName(name ++ NameTransformer.NAME_JOIN_STRING)))
/*
    /** Name with variance prefix: `+` for covariant, `-` for contravariant */
    def withVariance(v: Int): N =
      if (hasVariance) dropVariance.withVariance(v)
      else v match {
        case -1 => likeTyped('-' +: name)
        case  1 => likeTyped('+' +: name)
        case  0 => name
      }

    /** Does name have a `+`/`-` variance prefix? */
    def hasVariance: Boolean =
      name.nonEmpty && name.head == '+' || name.head == '-'

    /** Drop variance prefix if name has one */
    def dropVariance: N = if (hasVariance) likeTyped(name.tail) else name

    /** The variance as implied by the variance prefix, or 0 if there is
     *  no variance prefix.
     */
    def variance = name.head match {
      case '-' => -1
      case '+' => 1
      case _ => 0
    }

*/
    def unmangleClassName: N =
      if (Config.semanticNames && name.isSimple && name.isTypeName)
        if (name.endsWith(MODULE_SUFFIX) && !tpnme.falseModuleClassNames.contains(name.asTypeName))
          likeTyped(name.dropRight(MODULE_SUFFIX.length).moduleClassName)
        else name
      else name

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

    /** Is a synthetic function name
     *    - N for FunctionN
     *    - N for ImplicitFunctionN
      *   - (-1) otherwise
     */
    def functionArity: Int =
      functionArityFor(tpnme.Function) max functionArityFor(tpnme.ImplicitFunction)

    /** Is a function name
     *    - FunctionN for N >= 0
     *    - ImplicitFunctionN for N >= 0
     *    - false otherwise
     */
    def isFunction: Boolean = functionArity >= 0

    /** Is a implicit function name
     *    - ImplicitFunctionN for N >= 0
     *    - false otherwise
     */
    def isImplicitFunction: Boolean = functionArityFor(tpnme.ImplicitFunction) >= 0

    /** Is a synthetic function name
     *    - FunctionN for N > 22
     *    - ImplicitFunctionN for N >= 0
     *    - false otherwise
     */
    def isSyntheticFunction: Boolean = {
      functionArityFor(tpnme.Function) > MaxImplementedFunctionArity ||
        functionArityFor(tpnme.ImplicitFunction) >= 0
    }

    /** Parsed function arity for function with some specific prefix */
    private def functionArityFor(prefix: Name): Int = {
      if (name.startsWith(prefix))
        try name.toString.substring(prefix.length).toInt
        catch { case _: NumberFormatException => -1 }
      else -1
    }

    /** The number of hops specified in an outer-select name */
    def outerSelectHops: Int = {
      require(isOuterSelect)
      name.dropRight(nme.OUTER_SELECT.length).toString.toInt
    }

    /** The name of the generic runtime operation corresponding to an array operation */
    def genericArrayOp: TermName = name match {
      case nme.apply => nme.array_apply
      case nme.length => nme.array_length
      case nme.update => nme.array_update
      case nme.clone_ => nme.array_clone
    }

    /** The name of the primitive runtime operation corresponding to an array operation */
    def primitiveArrayOp: TermName = name match {
      case nme.apply => nme.primitive.arrayApply
      case nme.length => nme.primitive.arrayLength
      case nme.update => nme.primitive.arrayUpdate
      case nme.clone_ => nme.clone_
    }

    def specializedFor(classTargs: List[Types.Type], classTargsNames: List[Name], methodTargs: List[Types.Type], methodTarsNames: List[Name])(implicit ctx: Context): name.ThisName = {

      def typeToTag(tp: Types.Type): Name = {
        tp.classSymbol match {
          case t if t eq defn.IntClass     => nme.specializedTypeNames.Int
          case t if t eq defn.BooleanClass => nme.specializedTypeNames.Boolean
          case t if t eq defn.ByteClass    => nme.specializedTypeNames.Byte
          case t if t eq defn.LongClass    => nme.specializedTypeNames.Long
          case t if t eq defn.ShortClass   => nme.specializedTypeNames.Short
          case t if t eq defn.FloatClass   => nme.specializedTypeNames.Float
          case t if t eq defn.UnitClass    => nme.specializedTypeNames.Void
          case t if t eq defn.DoubleClass  => nme.specializedTypeNames.Double
          case t if t eq defn.CharClass    => nme.specializedTypeNames.Char
          case _                           => nme.specializedTypeNames.Object
        }
      }

      val methodTags: Seq[Name] = (methodTargs zip methodTarsNames).sortBy(_._2).map(x => typeToTag(x._1))
      val classTags: Seq[Name] = (classTargs zip classTargsNames).sortBy(_._2).map(x => typeToTag(x._1))

      name.likeKinded(name ++ nme.specializedTypeNames.prefix ++
        methodTags.fold(nme.EMPTY)(_ ++ _) ++ nme.specializedTypeNames.separator ++
        classTags.fold(nme.EMPTY)(_ ++ _) ++ nme.specializedTypeNames.suffix)
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

    def setterName: TermName =
      if (name.isFieldName) name.fieldToGetter.setterName
      else name ++ SETTER_SUFFIX

    def getterName: TermName =
      if (name.isFieldName) fieldToGetter
      else setterToGetter

    def fieldName: TermName =
      if (name.isSetterName) {
        if (name.isTraitSetterName) {
          // has form <$-separated-trait-name>$_setter_$ `name`_$eq
          val start = name.lastPart.indexOfSlice(TRAIT_SETTER_SEPARATOR) + TRAIT_SETTER_SEPARATOR.length
          val end = name.lastPart.indexOfSlice(SETTER_SUFFIX)
          name.mapLast(n => (n.slice(start, end) ++ LOCAL_SUFFIX).asSimpleName)
        } else getterName.fieldName
      }
      else name.mapLast(n => (n ++ LOCAL_SUFFIX).asSimpleName)

    private def setterToGetter: TermName = {
      assert(name.endsWith(SETTER_SUFFIX), name + " is referenced as a setter but has wrong name format")
      name.mapLast(n => n.take(n.length - SETTER_SUFFIX.length).asSimpleName)
    }

    def fieldToGetter: TermName = {
      assert(name.isFieldName)
      name.mapLast(n => n.take(n.length - LOCAL_SUFFIX.length).asSimpleName)
    }

    /** Nominally, name$default$N, encoded for <init>
     *  @param  Post the parameters position.
     *  @note Default getter name suffixes start at 1, so `pos` has to be adjusted by +1
     */
    def defaultGetterName(pos: Int): TermName =
      if (Config.semanticNames) name.derived(NameInfo.DefaultGetter(pos))
      else {
        val prefix = if (name.isConstructorName) DEFAULT_GETTER_INIT else name
        prefix ++ DEFAULT_GETTER ++ (pos + 1).toString
      }

    /** Nominally, name from name$default$N, CONSTRUCTOR for <init> */
    def defaultGetterToMethod: TermName =
      if (Config.semanticNames)
        name rewrite {
          case DerivedTermName(methName, NameInfo.DefaultGetter(_)) => methName
        }
      else defaultGetterToMethodOfMangled

    def defaultGetterToMethodOfMangled: TermName = {
        val p = name.indexOfSlice(DEFAULT_GETTER)
        if (p >= 0) {
          val q = name.take(p).asTermName
          // i.e., if (q.decoded == CONSTRUCTOR.toString) CONSTRUCTOR else q
          if (q == DEFAULT_GETTER_INIT) CONSTRUCTOR else q
        } else name
    }

    /** If this is a default getter, its index (starting from 0), else -1 */
    def defaultGetterIndex: Int =
      if (Config.semanticNames)
        name collect {
          case DerivedTermName(methName, NameInfo.DefaultGetter(num)) => num
        } getOrElse -1
      else defaultGetterIndexOfMangled

    def defaultGetterIndexOfMangled: Int = {
      var i = name.length
      while (i > 0 && name(i - 1).isDigit) i -= 1
      if (i > 0 && i < name.length && name.take(i).endsWith(DEFAULT_GETTER))
        name.drop(i).toString.toInt - 1
      else
        -1
    }

    def stripScala2LocalSuffix: TermName =
      if (name.isScala2LocalSuffix) name.init.asTermName else name

    /** The name of an accessor for protected symbols. */
    def protectedAccessorName: TermName =
      PROTECTED_PREFIX ++ name.unexpandedName

    /** The name of a setter for protected symbols. Used for inherited Java fields. */
    def protectedSetterName: TermName =
      PROTECTED_SET_PREFIX ++ name.unexpandedName

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

    def lazyLocalName = name ++ nme.LAZY_LOCAL
    def nonLazyName = {
      assert(name.isLazyLocal)
      name.dropRight(nme.LAZY_LOCAL.length)
    }

    def inlineAccessorName = nme.INLINE_ACCESSOR_PREFIX ++ name ++ "$"

    def unmangleMethodName: TermName =
      if (Config.semanticNames && name.isSimple) {
        val idx = name.defaultGetterIndexOfMangled
        if (idx >= 0) name.defaultGetterToMethodOfMangled.defaultGetterName(idx)
        else name
      }
      else name

    def unmangleSuperName: TermName =
      if (Config.semanticNames && name.isSimple)
        SuperAccessorName.tryUnmangle(name.lastPart) match {
          case scala.Some(original) =>
            name.mapLast(_ => original.asSimpleName).derived(NameInfo.SuperAccessor)
          case None =>
            name
        }
      else name
  }

  private final val FalseSuper = "$$super".toTermName
  private val FalseSuperLength = FalseSuper.length
}
