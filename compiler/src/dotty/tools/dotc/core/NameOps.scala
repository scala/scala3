package dotty.tools.dotc
package core

import java.security.MessageDigest
import scala.io.Codec
import Names._, StdNames._, Contexts._, Symbols._, Flags._, NameKinds._, Types._
import scala.tasty.util.Chars
import Chars.isOperatorPart
import Definitions._

object NameOps {

  object compactify {
    lazy val md5: MessageDigest = MessageDigest.getInstance("MD5")

    final val CLASSFILE_NAME_CHAR_LIMIT = 240

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
    def apply(s: String): String = {
      val marker = "$$$$"

      val MaxNameLength = (CLASSFILE_NAME_CHAR_LIMIT - 6) min
        2 * (CLASSFILE_NAME_CHAR_LIMIT - 6 - 2 * marker.length - 32)

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

  implicit class NameDecorator[N <: Name](private val name: N) extends AnyVal {
    import nme._

    def testSimple(f: SimpleName => Boolean): Boolean = name match {
      case name: SimpleName => f(name)
      case name: TypeName => name.toTermName.testSimple(f)
      case _ => false
    }

    private def likeSpacedN(n: Name): N =
      name.likeSpaced(n).asInstanceOf[N]

    def isConstructorName: Boolean = name == CONSTRUCTOR || name == TRAIT_CONSTRUCTOR
    def isStaticConstructorName: Boolean = name == STATIC_CONSTRUCTOR
    def isLocalDummyName: Boolean = name startsWith str.LOCALDUMMY_PREFIX
    def isReplWrapperName: Boolean = name.toString contains str.REPL_SESSION_LINE
    def isReplAssignName: Boolean = name.toString contains str.REPL_ASSIGN_SUFFIX
    def isSetterName: Boolean = name endsWith str.SETTER_SUFFIX
    def isScala2LocalSuffix: Boolean = testSimple(_.endsWith(" "))
    def isSelectorName: Boolean = testSimple(n => n.startsWith("_") && n.drop(1).forall(_.isDigit))
    def isAnonymousClassName: Boolean = name.startsWith(str.ANON_CLASS)
    def isAnonymousFunctionName: Boolean = name.startsWith(str.ANON_FUN)
    def isUnapplyName: Boolean = name == nme.unapply || name == nme.unapplySeq

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

    /** is this the name of an object enclosing packagel-level definitions? */
    def isPackageObjectName: Boolean = name match {
      case name: TermName => name == nme.PACKAGE || name.endsWith(str.TOPLEVEL_SUFFIX)
      case name: TypeName =>
        name.toTermName match {
          case ModuleClassName(original) => original.isPackageObjectName
          case _ => false
        }
    }

    /** Convert this module name to corresponding module class name */
    def moduleClassName: TypeName = name.derived(ModuleClassName).toTypeName

    /** Convert this module class name to corresponding source module name */
    def sourceModuleName: TermName = name.toTermName.exclude(ModuleClassName)

    /** If name ends in module class suffix, drop it. This
     *  method needs to work on mangled as well as unmangled names because
     *  it is also called from the backend.
     */
    def stripModuleClassSuffix: N = likeSpacedN {
      val semName = name.toTermName match {
        case name: SimpleName if name.endsWith("$") => name.unmangleClassName
        case _ => name
      }
      semName.exclude(ModuleClassName)
    }

    /** If flags is a ModuleClass but not a Package, add module class suffix */
    def adjustIfModuleClass(flags: FlagSet): N = likeSpacedN {
      if (flags is (ModuleClass, butNot = Package)) name.asTypeName.moduleClassName
      else name.toTermName.exclude(AvoidClashName)
    }

    /** The expanded name.
     *  This is the fully qualified name of `base` with `ExpandPrefixName` as separator,
     *  followed by `kind` and the name.
     */
    def expandedName(base: Symbol, kind: QualifiedNameKind = ExpandedName)(implicit ctx: Context): N =
      likeSpacedN { base.fullNameSeparated(ExpandPrefixName, kind, name) }

    /** Revert the expanded name. */
    def unexpandedName: N = likeSpacedN {
      name.replace { case ExpandedName(_, unexp) => unexp }
    }

    /** Remove the variance from the name. */
    def invariantName: N = likeSpacedN {
      name.replace { case VariantName(invariant, _) => invariant }
    }

    def errorName: N = likeSpacedN(name ++ nme.ERROR)

    /** Map variance value -1, +1 to 0, 1 */
    private def varianceToNat(v: Int) = (v + 1) / 2

    /** Map 0, 1 to variance value -1, +1 */
    private def natToVariance(n: Int) = n * 2 - 1

    /** Name with variance prefix: `+` for covariant, `-` for contravariant */
    def withVariance(v: Int): N = {
      val underlying = name.exclude(VariantName)
      likeSpacedN(
          if (v == 0) underlying
          else VariantName(underlying.toTermName, varianceToNat(v)))
    }

    /** The variance as implied by the variance prefix, or 0 if there is
     *  no variance prefix.
     */
    def variance: Int = name.collect { case VariantName(_, n) => natToVariance(n) }.getOrElse(0)

    def freshened(implicit ctx: Context): N = likeSpacedN {
      name.toTermName match {
        case ModuleClassName(original) => ModuleClassName(original.freshened)
        case name => UniqueName.fresh(name)
      }
    }

    def functionArity: Int =
      functionArityFor(str.Function) max
      functionArityFor(str.ImplicitFunction) max {
        val n =
          functionArityFor(str.ErasedFunction) max
          functionArityFor(str.ErasedImplicitFunction)
        if (n == 0) -1 else n
      }

    /** Is a function name, i.e one of FunctionXXL, FunctionN, ImplicitFunctionN for N >= 0 or ErasedFunctionN, ErasedImplicitFunctionN for N > 0
     */
    def isFunction: Boolean = (name eq tpnme.FunctionXXL) || functionArity >= 0

    /** Is an implicit function name, i.e one of ImplicitFunctionN for N >= 0 or ErasedImplicitFunctionN for N > 0
     */
    def isImplicitFunction: Boolean = {
      functionArityFor(str.ImplicitFunction) >= 0 ||
      functionArityFor(str.ErasedImplicitFunction) > 0
    }

    /** Is an erased function name, i.e. one of ErasedFunctionN, ErasedImplicitFunctionN for N > 0
      */
    def isErasedFunction: Boolean = {
      functionArityFor(str.ErasedFunction) > 0 ||
      functionArityFor(str.ErasedImplicitFunction) > 0
    }

    /** Is a synthetic function name, i.e. one of
     *    - FunctionN for N > 22
     *    - ImplicitFunctionN for N >= 0
     *    - ErasedFunctionN for N > 0
     *    - ErasedImplicitFunctionN for N > 0
     */
    def isSyntheticFunction: Boolean = {
      functionArityFor(str.Function) > MaxImplementedFunctionArity ||
      functionArityFor(str.ImplicitFunction) >= 0 ||
      isErasedFunction
    }

    /** Parsed function arity for function with some specific prefix */
    private def functionArityFor(prefix: String): Int = {
      if (name.startsWith(prefix)) {
        val suffix = name.toString.substring(prefix.length)
        if (suffix.matches("\\d+"))
          suffix.toInt
        else
          -1
      } else -1
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

    def specializedFor(classTargs: List[Type], classTargsNames: List[Name], methodTargs: List[Type], methodTarsNames: List[Name])(implicit ctx: Context): N = {

      val methodTags: Seq[Name] = (methodTargs zip methodTarsNames).sortBy(_._2).map(x => defn.typeTag(x._1))
      val classTags: Seq[Name] = (classTargs zip classTargsNames).sortBy(_._2).map(x => defn.typeTag(x._1))

      likeSpacedN(name ++ nme.specializedTypeNames.prefix ++
        methodTags.fold(nme.EMPTY)(_ ++ _) ++ nme.specializedTypeNames.separator ++
        classTags.fold(nme.EMPTY)(_ ++ _) ++ nme.specializedTypeNames.suffix)
    }

    /** If name length exceeds allowable limit, replace part of it by hash */
    def compactified(implicit ctx: Context): TermName = termName(compactify(name.toString))

    def unmangleClassName: N = name.toTermName match {
      case name: SimpleName
      if name.endsWith(str.MODULE_SUFFIX) && !nme.falseModuleClassNames.contains(name) =>
        likeSpacedN(name.dropRight(str.MODULE_SUFFIX.length).moduleClassName)
      case _ => name
    }

    def unmangle(kind: NameKind): N = likeSpacedN {
      name replace {
        case unmangled: SimpleName =>
          kind.unmangle(unmangled)
        case ExpandedName(prefix, last) =>
          kind.unmangle(last) replace {
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

  implicit class TermNameDecorator(private val name: TermName) extends AnyVal {
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
