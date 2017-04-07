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

    def testSimple(f: SimpleName => Boolean): Boolean = name match {
      case name: SimpleName => f(name)
      case name: TypeName => name.toTermName.testSimple(f)
      case _ => false
    }

    def likeSpaced(n: PreName): N =
      (if (name.isTermName) n.toTermName else n.toTypeName).asInstanceOf[N]

    def isConstructorName = name == CONSTRUCTOR || name == TRAIT_CONSTRUCTOR
    def isStaticConstructorName = name == STATIC_CONSTRUCTOR
    def isLocalDummyName = name startsWith str.LOCALDUMMY_PREFIX
    def isReplWrapperName = name.toString contains str.INTERPRETER_IMPORT_WRAPPER
    def isSetterName = name endsWith str.SETTER_SUFFIX
    def isScala2LocalSuffix = testSimple(_.endsWith(" "))
    def isSelectorName = testSimple(n => n.startsWith("_") && n.drop(1).forall(_.isDigit))

    /** Is name a variable name? */
    def isVariableName: Boolean = testSimple { n =>
      n.length > 0 && {
        val first = n.head
        (((first.isLower && first.isLetter) || first == '_')
          && (n != false_)
          && (n != true_)
          && (n != null_))
      }
    }

    def isOpAssignmentName: Boolean = name match {
      case raw.NE | raw.LE | raw.GE | EMPTY =>
        false
      case name: SimpleName =>
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
    def stripModuleClassSuffix: N = likeSpaced {
      val semName = name.toTermName match {
        case name: SimpleName if name.endsWith("$") => name.unmangleClassName
        case _ => name
      }
      semName.exclude(ModuleClassName)
    }

    /** If flags is a ModuleClass but not a Package, add module class suffix */
    def adjustIfModuleClass(flags: Flags.FlagSet): N = likeSpaced {
      if (flags is (ModuleClass, butNot = Package)) name.asTypeName.moduleClassName
      else name.toTermName.exclude(AvoidClashName)
    }

    def expandedName(base: Symbol, kind: QualifiedNameKind = ExpandedName)(implicit ctx: Context): N = {
      val prefix =
        if (base.name.is(ExpandedName)) base.name else base.fullNameSeparated(ExpandPrefixName)
      likeSpaced { kind(prefix.toTermName, name.toTermName) }
    }

    /** Revert the expanded name. */
    def unexpandedName: N = likeSpaced {
      name.rewrite { case ExpandedName(_, unexp) => unexp }
    }

    def implClassName: N = likeSpaced(name ++ tpnme.IMPL_CLASS_SUFFIX)

    def errorName: N = likeSpaced(name ++ nme.ERROR)


    /** Name with variance prefix: `+` for covariant, `-` for contravariant */
    def withVariance(v: Int): N =
      likeSpaced { VariantName(name.exclude(VariantName).toTermName, v) }

    /** The variance as implied by the variance prefix, or 0 if there is
     *  no variance prefix.
     */
    def variance = name.collect { case VariantName(_, n) => n }.getOrElse(0)

    def freshened(implicit ctx: Context): N = likeSpaced {
      name.toTermName match {
        case ModuleClassName(original) => ModuleClassName(original.freshened)
        case name => UniqueName.fresh(name)
      }
    }

    /** Is a synthetic function name
     *    - N for FunctionN
     *    - N for ImplicitFunctionN
      *   - (-1) otherwise
     */
    def functionArity: Int =
      functionArityFor(str.Function) max functionArityFor(str.ImplicitFunction)

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
    def isImplicitFunction: Boolean = functionArityFor(str.ImplicitFunction) >= 0

    /** Is a synthetic function name
     *    - FunctionN for N > 22
     *    - ImplicitFunctionN for N >= 0
     *    - false otherwise
     */
    def isSyntheticFunction: Boolean = {
      functionArityFor(str.Function) > MaxImplementedFunctionArity ||
        functionArityFor(str.ImplicitFunction) >= 0
    }

    /** Parsed function arity for function with some specific prefix */
    private def functionArityFor(prefix: String): Int = {
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

      name.likeSpaced(name ++ nme.specializedTypeNames.prefix ++
        methodTags.fold(nme.EMPTY)(_ ++ _) ++ nme.specializedTypeNames.separator ++
        classTags.fold(nme.EMPTY)(_ ++ _) ++ nme.specializedTypeNames.suffix)
    }

    /** If name length exceeds allowable limit, replace part of it by hash */
    def compactified(implicit ctx: Context): TermName = termName(compactify(name.toString))

    def unmangleClassName: N = name.toTermName match {
      case name: SimpleName
      if name.endsWith(str.MODULE_SUFFIX) && !nme.falseModuleClassNames.contains(name) =>
        likeSpaced(name.dropRight(str.MODULE_SUFFIX.length).moduleClassName)
      case _ => name
    }

    def unmangle(kind: NameKind): N = likeSpaced {
      name rewrite {
        case unmangled: SimpleName =>
          kind.unmangle(unmangled)
        case ExpandedName(prefix, last) =>
          kind.unmangle(last) rewrite {
            case kernel: SimpleName =>
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

    def setterName: TermName = name.exclude(FieldName) ++ str.SETTER_SUFFIX

    def getterName: TermName =
      name.exclude(FieldName).mapLast(n =>
        if (n.endsWith(str.SETTER_SUFFIX)) n.take(n.length - str.SETTER_SUFFIX.length).asSimpleName
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
      if (name.isScala2LocalSuffix) name.asSimpleName.dropRight(1) else name

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
