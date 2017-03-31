package dotty.tools.dotc
package core

import java.security.MessageDigest
import scala.annotation.switch
import scala.io.Codec
import Names._, StdNames._, Contexts._, Symbols._, Flags._, NameKinds._
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

  implicit class NameDecorator[N <: Name](val name: N) extends AnyVal {
    import nme._

    def likeTyped(n: PreName): N =
      (if (name.isTermName) n.toTermName else n.toTypeName).asInstanceOf[N]

    def isConstructorName = name == CONSTRUCTOR || name == TRAIT_CONSTRUCTOR
    def isStaticConstructorName = name == STATIC_CONSTRUCTOR
    def isLocalDummyName = name startsWith LOCALDUMMY_PREFIX
    def isReplWrapperName = name.toSimpleName containsSlice INTERPRETER_IMPORT_WRAPPER
    def isSetterName = name endsWith SETTER_SUFFIX
    def isScala2LocalSuffix = name.endsWith(" ")
    def isSelectorName = name.startsWith("_") && name.tail.forall(_.isDigit)

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

    /** Convert this module name to corresponding module class name */
    def moduleClassName: TypeName = name.derived(ModuleClassName).toTypeName

    /** Convert this module class name to corresponding source module name */
    def sourceModuleName: TermName = name.toTermName.exclude(ModuleClassName)

    /** If name ends in module class suffix, drop it. This
     *  method needs to work on mangled as well as unmangled names because
     *  it is also called from the backend.
     */
    def stripModuleClassSuffix: Name = name.toTermName match {
      case n: SimpleTermName if n.endsWith("$") =>
        name.unmangleClassName.exclude(ModuleClassName)
      case _ =>
        name.exclude(ModuleClassName)
    }

    /** If flags is a ModuleClass but not a Package, add module class suffix */
    def adjustIfModuleClass(flags: Flags.FlagSet): N = likeTyped {
      if (flags is (ModuleClass, butNot = Package)) name.asTypeName.moduleClassName
      else name.toTermName.exclude(AvoidClashName)
    }

    def expandedName(base: Symbol, kind: QualifiedNameKind = ExpandedName)(implicit ctx: Context): N = {
      val prefix =
        if (base.name.is(ExpandedName)) base.name else base.fullNameSeparated(ExpandPrefixName)
      likeTyped { kind(prefix.toTermName, name.toTermName) }
    }

    /** Revert the expanded name. */
    def unexpandedName: N = likeTyped {
      name.rewrite { case ExpandedName(_, unexp) => unexp }
    }

    def implClassName: N = likeTyped(name ++ tpnme.IMPL_CLASS_SUFFIX)

    def errorName: N = likeTyped(name ++ nme.ERROR)

    def directName: N = likeTyped(name ++ DIRECT_SUFFIX)

    def freshened(implicit ctx: Context): N =
      likeTyped(
        if (name.is(ModuleClassName)) name.stripModuleClassSuffix.freshened.moduleClassName
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
    def freshened(implicit ctx: Context): N = likeTyped {
      name.toTermName match {
        case ModuleClassName(original) => ModuleClassName(original.freshened)
        case name => UniqueName.fresh(name)
      }
    }

    def unmangleClassName: N =
      if (name.isSimple && name.isTypeName)
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

    def unmangle(kind: NameKind): N = likeTyped {
      name rewrite {
        case unmangled: SimpleTermName =>
          kind.unmangle(unmangled)
        case ExpandedName(prefix, last) =>
          kind.unmangle(last) rewrite {
            case kernel: SimpleTermName =>
              ExpandedName(prefix, kernel)
          }
      }
    }

    def unmangle(kinds: List[NameKind]): N = {
      val unmangled = (name /: kinds)(_.unmangle(_))
      if (unmangled eq name) name else unmangled.unmangle(kinds)
    }
  }

  implicit class TermNameDecorator(val name: TermName) extends AnyVal {
    import nme._

    def setterName: TermName = name.exclude(FieldName) ++ SETTER_SUFFIX

    def getterName: TermName =
      name.exclude(FieldName).mapLast(n =>
        if (n.endsWith(SETTER_SUFFIX)) n.take(n.length - SETTER_SUFFIX.length).asSimpleName
        else n)

    def fieldName: TermName =
      if (name.isSetterName) {
        if (name.is(TraitSetterName)) {
          val TraitSetterName(_, original) = name
          original.fieldName
        }
        else getterName.fieldName
      }
      else FieldName(name)

    def stripScala2LocalSuffix: TermName =
      if (name.isScala2LocalSuffix) name.init.asTermName else name

    /** The name unary_x for a prefix operator x */
    def toUnaryName: TermName = name match {
      case raw.MINUS => UNARY_-
      case raw.PLUS  => UNARY_+
      case raw.TILDE => UNARY_~
      case raw.BANG  => UNARY_!
      case _ => name
    }
  }
}
