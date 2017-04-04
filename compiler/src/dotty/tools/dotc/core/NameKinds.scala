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

object NameKinds {

  // These are sharable since all NameKinds are created eagerly at the start of the program
  // before any concurrent threads are forked. for this to work, NameKinds should never
  // be created lazily or in modules that start running after compilers are forked.
  @sharable private val simpleNameKinds = new mutable.HashMap[Int, ClassifiedNameKind]
  @sharable private val qualifiedNameKinds = new mutable.HashMap[Int, QualifiedNameKind]
  @sharable private val uniqueNameKinds = new mutable.HashMap[String, UniqueNameKind]

  abstract class NameInfo extends DotClass {
    def kind: NameKind
    def mkString(underlying: TermName): String
    def map(f: SimpleTermName => SimpleTermName): NameInfo = this
  }

  abstract class NameKind(val tag: Int) extends DotClass { self =>
    type ThisInfo <: Info
    class Info extends NameInfo { this: ThisInfo =>
      def kind = self
      def mkString(underlying: TermName) = self.mkString(underlying, this)
      override def toString = infoString
    }
    def definesNewName = false
    def unmangle(name: SimpleTermName): TermName = name
    def mkString(underlying: TermName, info: ThisInfo): String
    def infoString: String
  }

  object SimpleTermNameKind extends NameKind(UTF8) { self =>
    type ThisInfo = Info
    val info = new Info
    def mkString(underlying: TermName, info: ThisInfo) = unsupported("mkString")
    def infoString = unsupported("infoString")
  }

  abstract class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info = new Info
    def apply(qual: TermName) =
      qual.derived(info)
    def unapply(name: DerivedTermName): Option[TermName] =  name match {
      case DerivedTermName(underlying, `info`) => Some(underlying)
      case _ => None
    }
    simpleNameKinds(tag) = this
  }

  class PrefixNameKind(tag: Int, prefix: String, optInfoString: String = "")
  extends ClassifiedNameKind(tag, if (optInfoString.isEmpty) s"Prefix $prefix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo) =
      underlying.mapLast(n => termName(prefix + n.toString)).toString
    override def unmangle(name: SimpleTermName): TermName =
      if (name.startsWith(prefix)) apply(name.drop(prefix.length).asSimpleName)
      else name
  }

  class SuffixNameKind(tag: Int, suffix: String, optInfoString: String = "")
  extends ClassifiedNameKind(tag, if (optInfoString.isEmpty) s"Suffix $suffix" else optInfoString) {
    def mkString(underlying: TermName, info: ThisInfo) = underlying.toString ++ suffix
    override def unmangle(name: SimpleTermName): TermName =
      if (name.endsWith(suffix)) apply(name.take(name.length - suffix.length).asSimpleName)
      else name
  }

  trait QualifiedInfo extends NameInfo {
    val name: SimpleTermName
  }

  class QualifiedNameKind(tag: Int, val separator: String)
  extends NameKind(tag) {
    type ThisInfo = QualInfo
    case class QualInfo(val name: SimpleTermName) extends Info with QualifiedInfo {
      override def map(f: SimpleTermName => SimpleTermName): NameInfo = new QualInfo(f(name))
      override def toString = s"$infoString $name"
    }
    def apply(qual: TermName, name: SimpleTermName): TermName =
      qual.derived(new QualInfo(name))

    /** Overloaded version used only for ExpandedName and TraitSetterName.
     *  Needed because the suffix of an expanded name may itself be expanded.
     *  For example, look at javap of scala.App.initCode
     */
    def apply(qual: TermName, name: TermName): TermName = name rewrite {
      case name: SimpleTermName => apply(qual, name)
      case AnyQualifiedName(_, _) => apply(qual, name.toSimpleName)
    }

    def unapply(name: DerivedTermName): Option[(TermName, SimpleTermName)] = name match {
      case DerivedTermName(qual, info: this.QualInfo) => Some((qual, info.name))
      case _ => None
    }

    override def definesNewName = true

    def mkString(underlying: TermName, info: ThisInfo) =
      s"$underlying$separator${info.name}"
    def infoString = s"Qualified $separator"

    qualifiedNameKinds(tag) = this
  }

  object AnyQualifiedName {
    def unapply(name: DerivedTermName): Option[(TermName, SimpleTermName)] = name match {
      case DerivedTermName(qual, info: QualifiedInfo) =>
        Some((name.underlying, info.name))
      case _ => None
    }
  }

  trait NumberedInfo extends NameInfo {
    def num: Int
  }

  abstract class NumberedNameKind(tag: Int, val infoString: String) extends NameKind(tag) { self =>
    type ThisInfo = NumberedInfo
    case class NumberedInfo(val num: Int) extends Info with NameKinds.NumberedInfo {
      override def toString = s"$infoString $num"
    }
    def apply(qual: TermName, num: Int) =
      qual.derived(new NumberedInfo(num))
    def unapply(name: DerivedTermName): Option[(TermName, Int)] = name match {
      case DerivedTermName(underlying, info: this.NumberedInfo) => Some((underlying, info.num))
      case _ => None
    }
    protected def skipSeparatorAndNum(name: SimpleTermName, separator: String): Int = {
      var i = name.length
      while (i > 0 && name(i - 1).isDigit) i -= 1
      if (i > separator.length && i < name.length &&
          name.slice(i - separator.length, i).toString == separator) i
      else -1
    }
  }

  object AnyNumberedName {
    def unapply(name: DerivedTermName): Option[(TermName, Int)] = name match {
      case DerivedTermName(qual, info: NumberedInfo) => Some((qual, info.num))
      case _ => None
    }
  }

  case class UniqueNameKind(val separator: String)
  extends NumberedNameKind(UNIQUE, s"Unique $separator") {
    override def definesNewName = true
    def mkString(underlying: TermName, info: ThisInfo) = {
      val safePrefix = str.sanitize(underlying.toString + separator)
      safePrefix + info.num
    }

    def fresh(prefix: TermName = EmptyTermName)(implicit ctx: Context): TermName =
      ctx.freshNames.newName(prefix, this)

    uniqueNameKinds(separator) = this
  }

  object AnyUniqueName {
    def unapply(name: DerivedTermName): Option[(TermName, String, Int)] = name match {
      case DerivedTermName(qual, info: NumberedInfo) =>
        info.kind match {
          case unique: UniqueNameKind => Some((qual, unique.separator, info.num))
          case _ => None
        }
      case _ => None
    }
  }

  val QualifiedName           = new QualifiedNameKind(QUALIFIED, ".")
  val FlatName                = new QualifiedNameKind(FLATTENED, "$")
  val ExpandPrefixName        = new QualifiedNameKind(EXPANDPREFIX, "$")

  val ExpandedName = new QualifiedNameKind(EXPANDED, str.EXPAND_SEPARATOR) {
    private val FalseSuper = termName("$$super")
    private val FalseSuperLength = FalseSuper.length

    override def unmangle(name: SimpleTermName): TermName = {
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

  val TraitSetterName         = new QualifiedNameKind(TRAITSETTER, str.TRAIT_SETTER_SEPARATOR)

  val UniqueName = new UniqueNameKind("$") {
    override def mkString(underlying: TermName, info: ThisInfo) =
      if (underlying.isEmpty) "$" + info.num + "$" else super.mkString(underlying, info)
  }

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

  val UniqueExtMethName = new UniqueNameKind("$extension") {
    override def unmangle(name: SimpleTermName): TermName = {
      val i = skipSeparatorAndNum(name, separator)
      if (i > 0) {
        val index = name.drop(i).toString.toInt
        val original = name.take(i - separator.length).asTermName
        apply(original, index)
      }
      else name
    }
  }

  val PatMatStdBinderName     = new UniqueNameKind("x")
  val PatMatPiName            = new UniqueNameKind("pi") // FIXME: explain what this is
  val PatMatPName             = new UniqueNameKind("p")  // FIXME: explain what this is
  val PatMatOName             = new UniqueNameKind("o")  // FIXME: explain what this is
  val PatMatCaseName          = new UniqueNameKind("case")
  val PatMatMatchFailName     = new UniqueNameKind("matchFail")
  val PatMatSelectorName      = new UniqueNameKind("selector")

  object DefaultGetterName extends NumberedNameKind(DEFAULTGETTER, "DefaultGetter") {
    def mkString(underlying: TermName, info: ThisInfo) = {
      val prefix = if (underlying.isConstructorName) nme.DEFAULT_GETTER_INIT else underlying
      prefix.toString + str.DEFAULT_GETTER + (info.num + 1)
    }
    // TODO: Reduce code duplication with UniqueExtMethName
    override def unmangle(name: SimpleTermName): TermName = {
      val i = skipSeparatorAndNum(name, str.DEFAULT_GETTER)
      if (i > 0) {
        val index = name.drop(i).toString.toInt - 1
        var original = name.take(i - str.DEFAULT_GETTER.length).asTermName
        if (original == nme.DEFAULT_GETTER_INIT) original = Names.CONSTRUCTOR
        apply(original, index)
      }
      else name
    }
  }

  object VariantName extends NumberedNameKind(VARIANT, "Variant") {
    val varianceToPrefix = Map(-1 -> '-', 0 -> '=', 1 -> '+')
    def mkString(underlying: TermName, info: ThisInfo) = {
      varianceToPrefix(info.num).toString + underlying
    }
  }

  /** Names of the form N_<outer>. Emitted by inliner, replaced by outer path
   *  in ExplicitOuter.
   */
  object OuterSelectName extends NumberedNameKind(OUTERSELECT, "OuterSelect") {
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
  val FieldName = new SuffixNameKind(FIELD, "$$local")
  val ExtMethName = new SuffixNameKind(EXTMETH, "$extension")
  val ModuleVarName = new SuffixNameKind(OBJECTVAR, "$module")
  val ModuleClassName = new SuffixNameKind(OBJECTCLASS, "$", optInfoString = "ModuleClass")

  object SignedName extends NameKind(63) {

    /** @param parts  resultSig followed by paramsSig */
    case class SignedInfo(sig: Signature) extends Info {
      override def toString = s"$infoString $sig"
    }
    type ThisInfo = SignedInfo

    def apply(qual: TermName, sig: Signature) =
      qual.derived(new SignedInfo(sig))
    def unapply(name: DerivedTermName): Option[(TermName, Signature)] = name match {
      case DerivedTermName(underlying, info: SignedInfo) => Some((underlying, info.sig))
      case _ => None
    }

    def mkString(underlying: TermName, info: ThisInfo): String = s"$underlying[with sig ${info.sig}]"
    def infoString: String = "Signed"
  }

  val Scala2MethodNameKinds: List[NameKind] =
    List(DefaultGetterName, ExtMethName, UniqueExtMethName, ProtectedAccessorName, ProtectedSetterName)

  def simpleNameKindOfTag      : collection.Map[Int, ClassifiedNameKind] = simpleNameKinds
  def qualifiedNameKindOfTag   : collection.Map[Int, QualifiedNameKind]  = qualifiedNameKinds
  def uniqueNameKindOfSeparator: collection.Map[String, UniqueNameKind]  = uniqueNameKinds
}