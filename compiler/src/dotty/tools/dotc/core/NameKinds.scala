package dotty.tools
package dotc
package core

import Names._
import NameOps._
import StdNames._
import util.DotClass
import tasty.TastyFormat._
import Decorators._
import Contexts.Context
import collection.mutable

/** Defines possible kinds of NameInfo of a derived name */
object NameKinds {

  // These are sharable since all NameKinds are created eagerly at the start of the program
  // before any concurrent threads are forked. for this to work, NameKinds should never
  // be created lazily or in modules that start running after compilers are forked.
  @sharable private val simpleNameKinds = new mutable.HashMap[Int, ClassifiedNameKind]
  @sharable private val qualifiedNameKinds = new mutable.HashMap[Int, QualifiedNameKind]
  @sharable private val numberedNameKinds = new mutable.HashMap[Int, NumberedNameKind]
  @sharable private val uniqueNameKinds = new mutable.HashMap[String, UniqueNameKind]

  /** A class for the info stored in a derived name */
  abstract class NameInfo extends DotClass {
    def kind: NameKind
    def mkString(underlying: TermName): String
    def map(f: SimpleName => SimpleName): NameInfo = this
  }

  /** An abstract base class of classes that define the kind of a derived name info */
  abstract class NameKind(val tag: Int) extends DotClass { self =>

    /** The info class defined by this kind */
    type ThisInfo <: Info

    /** A simple info type; some subclasses of Kind define more refined versions */
    class Info extends NameInfo { this: ThisInfo =>
      def kind = self
      def mkString(underlying: TermName) = self.mkString(underlying, this)
      override def toString = infoString
    }

    /** Does this kind define logically a new name (respectively qualified name)?
     *  Tested by the `rewrite` and `collect` combinators of class `Name`.
     */
    def definesNewName = false
    def definesQualifiedName = false

    /** Unmangle simple name `name` into a name of this kind, or return
     *  original name if this is not possible.
     */
    def unmangle(name: SimpleName): TermName = name

    /** Turn a name of this kind consisting of an `underlying` prefix
     *  and the given `info` into a string.
     */
    def mkString(underlying: TermName, info: ThisInfo): String

    /** A string used for displaying the structure of a name */
    def infoString: String
  }

  object SimpleNameKind extends NameKind(UTF8) { self =>
    type ThisInfo = Info
    val info = new Info
    def mkString(underlying: TermName, info: ThisInfo) = unsupported("mkString")
    def infoString = unsupported("infoString")
  }

  /** The kind of names that add a simple classification to an underlying name.
   */
  abstract class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info = new Info

    /** Build a new name of this kind from an underlying name */
    def apply(underlying: TermName) = underlying.derived(info)

    /** Extractor operation for names of this kind */
    def unapply(name: DerivedName): Option[TermName] =  name match {
      case DerivedName(underlying, `info`) => Some(underlying)
      case _ => None
    }

    simpleNameKinds(tag) = this
  }

  /** The kind of names that get formed by adding a prefix to an underlying name */
  class PrefixNameKind(tag: Int, prefix: String, optInfoString: String = "")
  extends ClassifiedNameKind(tag, if (optInfoString.isEmpty) s"Prefix $prefix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo) =
      underlying.qualToString(_.toString, n => prefix + n.toString)
    override def unmangle(name: SimpleName): TermName =
      if (name.startsWith(prefix)) apply(name.drop(prefix.length).asSimpleName)
      else name
  }

  /** The kind of names that get formed by appending a suffix to an underlying name */
  class SuffixNameKind(tag: Int, suffix: String, optInfoString: String = "")
  extends ClassifiedNameKind(tag, if (optInfoString.isEmpty) s"Suffix $suffix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo) =
      underlying.qualToString(_.toString, n => n.toString + suffix)
    override def unmangle(name: SimpleName): TermName =
      if (name.endsWith(suffix)) apply(name.take(name.length - suffix.length).asSimpleName)
      else name
  }

  /** A base trait for infos that define an additional selector name */
  trait QualifiedInfo extends NameInfo {
    val name: SimpleName
  }

  /** The kind of qualified names, consisting of an underlying name as a prefix,
   *  followed by a separator, followed by a simple selector name.
   *
   *  A qualified names always constitutes a new name, different from its underlying name.
   */
  class QualifiedNameKind(tag: Int, val separator: String)
  extends NameKind(tag) {
    type ThisInfo = QualInfo
    case class QualInfo(val name: SimpleName) extends Info with QualifiedInfo {
      override def map(f: SimpleName => SimpleName): NameInfo = new QualInfo(f(name))
      override def toString = s"$infoString $name"
    }

    def apply(qual: TermName, name: SimpleName): TermName =
      qual.derived(new QualInfo(name))

    /** Overloaded version used only for ExpandedName and TraitSetterName.
     *  Needed because the suffix of an expanded name may itself be expanded.
     *  For example, look at javap of scala.App.initCode
     */
    def apply(qual: TermName, name: TermName): TermName = name rewrite {
      case name: SimpleName => apply(qual, name)
      case AnyQualifiedName(_, _) => apply(qual, name.toSimpleName)
    }

    def unapply(name: DerivedName): Option[(TermName, SimpleName)] = name match {
      case DerivedName(qual, info: this.QualInfo) => Some((qual, info.name))
      case _ => None
    }

    override def definesNewName = true
    override def definesQualifiedName = true

    def mkString(underlying: TermName, info: ThisInfo) =
      s"$underlying$separator${info.name}"

    def infoString = s"Qualified $separator"

    qualifiedNameKinds(tag) = this
  }

  /** An extractor for qualified names of an arbitrary kind */
  object AnyQualifiedName {
    def unapply(name: DerivedName): Option[(TermName, SimpleName)] = name match {
      case DerivedName(qual, info: QualifiedInfo) =>
        Some((name.underlying, info.name))
      case _ => None
    }
  }

  /** A base trait for infos that contain a number */
  trait NumberedInfo extends NameInfo {
    def num: Int
  }

  /** The kind of numbered names consisting of an underlying name and a number */
  abstract class NumberedNameKind(tag: Int, val infoString: String) extends NameKind(tag) { self =>
    type ThisInfo = NumberedInfo
    case class NumberedInfo(val num: Int) extends Info with NameKinds.NumberedInfo {
      override def toString = s"$infoString $num"
    }
    def apply(qual: TermName, num: Int) =
      qual.derived(new NumberedInfo(num))
    def unapply(name: DerivedName): Option[(TermName, Int)] = name match {
      case DerivedName(underlying, info: this.NumberedInfo) => Some((underlying, info.num))
      case _ => None
    }
    protected def skipSeparatorAndNum(name: SimpleName, separator: String): Int = {
      var i = name.length
      while (i > 0 && name(i - 1).isDigit) i -= 1
      if (i > separator.length && i < name.length &&
          name.slice(i - separator.length, i).toString == separator) i
      else -1
    }

    numberedNameKinds(tag) = this
  }

  /** An extractor for numbered names of arbitrary kind */
  object AnyNumberedName {
    def unapply(name: DerivedName): Option[(TermName, Int)] = name match {
      case DerivedName(qual, info: NumberedInfo) => Some((qual, info.num))
      case _ => None
    }
  }

  /** The kind of unique names that consist of an underlying name (can be empty),
   *  a separator indicating the class of unique name, and a unique number.
   *
   *  A unique names always constitutes a new name, different from its underlying name.
   */
  case class UniqueNameKind(val separator: String)
  extends NumberedNameKind(UNIQUE, s"Unique $separator") {
    override def definesNewName = true

    def mkString(underlying: TermName, info: ThisInfo) = {
      val safePrefix = str.sanitize(underlying.toString) + separator
      safePrefix + info.num
    }

    /** Generate fresh unique name of this kind with given prefix name */
    def fresh(prefix: TermName = EmptyTermName)(implicit ctx: Context): TermName =
      ctx.freshNames.newName(prefix, this)

    uniqueNameKinds(separator) = this
  }

  /** An extractor for unique names of arbitrary kind */
  object AnyUniqueName {
    def unapply(name: DerivedName): Option[(TermName, String, Int)] = name match {
      case DerivedName(qual, info: NumberedInfo) =>
        info.kind match {
          case unique: UniqueNameKind => Some((qual, unique.separator, info.num))
          case _ => None
        }
      case _ => None
    }
  }

  /** Names of the form `prefix . name` */
  val QualifiedName = new QualifiedNameKind(QUALIFIED, ".")

  /** Names of the form `prefix $ name` that are constructed as a result of flattening */
  val FlatName = new QualifiedNameKind(FLATTENED, "$")

  /** Names of the form `prefix $ name` that are prefixes of expanded names */
  val ExpandPrefixName = new QualifiedNameKind(EXPANDPREFIX, "$")

  /** Expanded names of the form `prefix $$ name`. */
  val ExpandedName = new QualifiedNameKind(EXPANDED, str.EXPAND_SEPARATOR) {
    private val FalseSuper = termName("$$super")
    private val FalseSuperLength = FalseSuper.length

    override def unmangle(name: SimpleName): TermName = {
      var i = name.lastIndexOfSlice(str.EXPAND_SEPARATOR)
      if (i < 0) name
      else {
        // Hack to make super accessors from traits work. They would otherwise fail because of #765
        // The problem is that in `x$$super$$plus` the expansion prefix needs to be `x`
        // instead of `x$$super`.
        if (i > FalseSuperLength && name.slice(i - FalseSuperLength, i) == FalseSuper)
          i -= FalseSuper.length

        apply(name.take(i).asTermName, name.drop(i + str.EXPAND_SEPARATOR.length).asSimpleName)
      }
    }
  }

  /** Expanded names of the form `prefix $_setter_$ name`. These only occur in Scala2. */
  val TraitSetterName = new QualifiedNameKind(TRAITSETTER, str.TRAIT_SETTER_SEPARATOR)

  /** Unique names of the form `prefix $ n` or `$ n $` */
  val UniqueName = new UniqueNameKind("$") {
    override def mkString(underlying: TermName, info: ThisInfo) =
      if (underlying.isEmpty) "$" + info.num + "$" else super.mkString(underlying, info)
  }

  /** Other unique names */
  val InlineAccessorName      = new UniqueNameKind("$_inlineAccessor_$")
  val TempResultName          = new UniqueNameKind("ev$")
  val EvidenceParamName       = new UniqueNameKind("evidence$")
  val DepParamName            = new UniqueNameKind("(param)")
  val LazyImplicitName        = new UniqueNameKind("$_lazy_implicit_$")
  val LazyLocalName           = new UniqueNameKind("$lzy")
  val LazyLocalInitName       = new UniqueNameKind("$lzyINIT")
  val LazyFieldOffsetName     = new UniqueNameKind("$OFFSET")
  val LazyBitMapName          = new UniqueNameKind(nme.BITMAP_PREFIX.toString)
  val NonLocalReturnKeyName   = new UniqueNameKind("nonLocalReturnKey")
  val WildcardParamName       = new UniqueNameKind("_$")
  val TailLabelName           = new UniqueNameKind("tailLabel")
  val ExceptionBinderName     = new UniqueNameKind("ex")
  val SkolemName              = new UniqueNameKind("?")
  val LiftedTreeName          = new UniqueNameKind("liftedTree")
  val SuperArgName            = new UniqueNameKind("$superArg$")

  /** A kind of unique extension methods; Unlike other unique names, these can be
   *  unmangled.
   */
  val UniqueExtMethName = new UniqueNameKind("$extension") {
    override def unmangle(name: SimpleName): TermName = {
      val i = skipSeparatorAndNum(name, separator)
      if (i > 0) {
        val index = name.drop(i).toString.toInt
        val original = name.take(i - separator.length).asTermName
        apply(original, index)
      }
      else name
    }
  }

  /** Kinds of unique names generated by the pattern matcher */
  val PatMatStdBinderName     = new UniqueNameKind("x")
  val PatMatPiName            = new UniqueNameKind("pi") // FIXME: explain what this is
  val PatMatPName             = new UniqueNameKind("p")  // FIXME: explain what this is
  val PatMatOName             = new UniqueNameKind("o")  // FIXME: explain what this is
  val PatMatCaseName          = new UniqueNameKind("case")
  val PatMatMatchFailName     = new UniqueNameKind("matchFail")
  val PatMatSelectorName      = new UniqueNameKind("selector")

  val LocalOptInlineLocalObj  = new UniqueNameKind("ilo")

  /** The kind of names of default argument getters */
  val DefaultGetterName = new NumberedNameKind(DEFAULTGETTER, "DefaultGetter") {
    def mkString(underlying: TermName, info: ThisInfo) = {
      val prefix = if (underlying.isConstructorName) nme.DEFAULT_GETTER_INIT else underlying
      prefix.toString + str.DEFAULT_GETTER + (info.num + 1)
    }
    // TODO: Reduce code duplication with UniqueExtMethName
    override def unmangle(name: SimpleName): TermName = {
      val i = skipSeparatorAndNum(name, str.DEFAULT_GETTER)
      if (i > 0) {
        val index = name.drop(i).toString.toInt - 1
        var original = name.take(i - str.DEFAULT_GETTER.length).asTermName
        if (original == nme.DEFAULT_GETTER_INIT) original = nme.CONSTRUCTOR
        apply(original, index)
      }
      else name
    }
  }

  /** The kind of names that also encode a variance: 0 for contravariance, 1 for covariance. */
  val VariantName = new NumberedNameKind(VARIANT, "Variant") {
    def mkString(underlying: TermName, info: ThisInfo) = "-+"(info.num).toString + underlying
  }

  /** Names of the form N_<outer>. Emitted by inliner, replaced by outer path
   *  in ExplicitOuter.
   */
  val OuterSelectName = new NumberedNameKind(OUTERSELECT, "OuterSelect") {
    def mkString(underlying: TermName, info: ThisInfo) = {
      assert(underlying.isEmpty)
      info.num + "_<outer>"
    }
  }

  val SuperAccessorName = new PrefixNameKind(SUPERACCESSOR, "super$")
  val InitializerName = new PrefixNameKind(INITIALIZER, "initial$")
  val ShadowedName = new PrefixNameKind(SHADOWED, "(shadowed)")
  val ProtectedAccessorName = new PrefixNameKind(PROTECTEDACCESSOR, "protected$")
  val ProtectedSetterName = new PrefixNameKind(PROTECTEDSETTER, "protected$set") // dubious encoding, kept for Scala2 compatibility
  val AvoidClashName = new SuffixNameKind(AVOIDCLASH, "$_avoid_name_clash_$")
  val DirectMethodName = new SuffixNameKind(DIRECT, "$direct") { override def definesNewName = true }
  val FieldName = new SuffixNameKind(FIELD, "$$local") {
      override def mkString(underlying: TermName, info: ThisInfo) = underlying.toString
  }
  val ExtMethName = new SuffixNameKind(EXTMETH, "$extension")
  val ModuleVarName = new SuffixNameKind(OBJECTVAR, "$module")
  val ModuleClassName = new SuffixNameKind(OBJECTCLASS, "$", optInfoString = "ModuleClass")
  val ImplMethName = new SuffixNameKind(IMPLMETH, "$")

  /** A name together with a signature. Used in Tasty trees. */
  object SignedName extends NameKind(63) {

    case class SignedInfo(sig: Signature) extends Info {
      override def toString = s"$infoString $sig"
    }
    type ThisInfo = SignedInfo

    def apply(qual: TermName, sig: Signature) =
      qual.derived(new SignedInfo(sig))
    def unapply(name: DerivedName): Option[(TermName, Signature)] = name match {
      case DerivedName(underlying, info: SignedInfo) => Some((underlying, info.sig))
      case _ => None
    }

    def mkString(underlying: TermName, info: ThisInfo): String = s"$underlying[with sig ${info.sig}]"
    def infoString: String = "Signed"
  }

  /** Possible name kinds of a method that comes from Scala2 pickling info. */
  val Scala2MethodNameKinds: List[NameKind] =
    List(DefaultGetterName, ExtMethName, UniqueExtMethName, ProtectedAccessorName, ProtectedSetterName)

  def simpleNameKindOfTag      : collection.Map[Int, ClassifiedNameKind] = simpleNameKinds
  def qualifiedNameKindOfTag   : collection.Map[Int, QualifiedNameKind]  = qualifiedNameKinds
  def numberedNameKindOfTag    : collection.Map[Int, NumberedNameKind]   = numberedNameKinds
  def uniqueNameKindOfSeparator: collection.Map[String, UniqueNameKind]  = uniqueNameKinds
}
