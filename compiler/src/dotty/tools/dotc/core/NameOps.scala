package dotty.tools
package dotc
package core

import java.security.MessageDigest
import java.nio.CharBuffer
import scala.io.Codec
import Int.MaxValue
import Names.*, StdNames.*, Contexts.*, Symbols.*, Flags.*, NameKinds.*, Types.*
import util.Chars.{isOperatorPart, digit2int}
import Definitions.*
import nme.*

object NameOps {

  object compactify {
    lazy val md5: MessageDigest = MessageDigest.getInstance("MD5").nn

    inline val CLASSFILE_NAME_CHAR_LIMIT = 240

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

      val MaxNameLength = (CLASSFILE_NAME_CHAR_LIMIT - 6).min(
        2 * (CLASSFILE_NAME_CHAR_LIMIT - 6 - 2 * marker.length - 32)
      )

      def toMD5(s: String, edge: Int): String = {
        val prefix = s.take(edge)
        val suffix = s.takeRight(edge)

        val cs = s.toArray
        val bytes = Codec.toUTF8(CharBuffer.wrap(cs).nn)
        md5.update(bytes)
        val md5chars = md5.digest().nn.map(b => (b & 0xFF).toHexString).mkString

        prefix + marker + md5chars + marker + suffix
      }

      if (s.length <= MaxNameLength) s else toMD5(s, MaxNameLength / 4)
    }
  }

  extension [N <: Name](name: N) {

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
    def isSetterName: Boolean = name.endsWith(str.SETTER_SUFFIX) || name.is(SyntheticSetterName)
    def isScala2LocalSuffix: Boolean = testSimple(_.endsWith(" "))
    def isSelectorName: Boolean = testSimple(n => n.startsWith("_") && n.drop(1).forall(_.isDigit))
    def isAnonymousClassName: Boolean = name.startsWith(str.ANON_CLASS)
    def isAnonymousFunctionName: Boolean = name.startsWith(str.ANON_FUN)
    def isUnapplyName: Boolean = name == nme.unapply || name == nme.unapplySeq
    def isRightAssocOperatorName: Boolean = name.lastPart.last == ':'

    def isOperatorName: Boolean = name match
      case name: SimpleName => name.exists(isOperatorPart)
      case _ => false

    /** Is name of a variable pattern? */
    def isVarPattern: Boolean =
      testSimple { n =>
        n.length > 0 && {
          def isLowerLetterSupplementary: Boolean =
            import Character.{isHighSurrogate, isLowSurrogate, isLetter, isLowerCase, isValidCodePoint, toCodePoint}
            isHighSurrogate(n(0)) && n.length > 1 && isLowSurrogate(n(1)) && {
              val codepoint = toCodePoint(n(0), n(1))
              isValidCodePoint(codepoint) && isLetter(codepoint) && isLowerCase(codepoint)
            }
          val first = n.head
          ((first.isLower && first.isLetter || first == '_' || isLowerLetterSupplementary)
            && n != false_
            && n != true_
            && n != null_)
        }
      } || name.is(PatMatGivenVarName)

    def isOpAssignmentName: Boolean = name match {
      case raw.NE | raw.LE | raw.GE | EMPTY =>
        false
      case name: SimpleName =>
        name.length > 0 && name.last == '=' && name.head != '=' && isOperatorPart(name.firstCodePoint)
      case _ =>
        false
    }

    /** is this the name of an object enclosing package-level definitions? */
    def isPackageObjectName: Boolean = name match {
      case name: TermName => name == nme.PACKAGE || name.endsWith(str.TOPLEVEL_SUFFIX)
      case name: TypeName =>
        name.toTermName match {
          case ModuleClassName(original) => original.isPackageObjectName
          case _ => false
        }
    }

    /** is this the name of an object enclosing top-level definitions? */
    def isTopLevelPackageObjectName: Boolean = name match {
      case name: TermName => name.endsWith(str.TOPLEVEL_SUFFIX)
      case name: TypeName =>
        name.toTermName match {
          case ModuleClassName(original) => original.isTopLevelPackageObjectName
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
      val semName = name.toTermName match
        case name: SimpleName if name.endsWith(str.MODULE_SUFFIX) && name.lastPart != MODULE_SUFFIX => name.unmangleClassName
        case _ => name
      semName.exclude(ModuleClassName)
    }

    /** If flags is a ModuleClass but not a Package, add module class suffix */
    def adjustIfModuleClass(flags: FlagSet): N = likeSpacedN {
      if (flags.is(ModuleClass, butNot = Package)) name.asTypeName.moduleClassName
      else name.toTermName
    }

    /** The expanded name.
     *  This is the fully qualified name of `base` with `ExpandPrefixName` as separator,
     *  followed by `kind` and the name.
     */
    def expandedName(base: Symbol, kind: QualifiedNameKind = ExpandedName)(using Context): N =
      likeSpacedN { base.fullNameSeparated(ExpandPrefixName, kind, name) }

    /** Revert the expanded name. */
    def unexpandedName: N = likeSpacedN {
      name.replaceDeep {
        case ExpandedName(_, unexp) => unexp
      }
    }

    def errorName: N = likeSpacedN(name ++ nme.ERROR)

    def freshened(using Context): N = likeSpacedN {
      name.toTermName match {
        case ModuleClassName(original) => ModuleClassName(original.freshened)
        case name => UniqueName.fresh(name)
      }
    }

    /** Do two target names match? An empty target name matchws any other name. */
    def matchesTargetName(other: Name) =
      name == other || name.isEmpty || other.isEmpty

    private def functionSuffixStart: Int =
      val first = name.firstPart
      var idx = first.length - 1
      if idx >= 8 && first(idx).isDigit then
        while
          idx = idx - 1
          idx >= 8 && first(idx).isDigit
        do ()
        if    first(idx - 7) == 'F'
           && first(idx - 6) == 'u'
           && first(idx - 5) == 'n'
           && first(idx - 4) == 'c'
           && first(idx - 3) == 't'
           && first(idx - 2) == 'i'
           && first(idx - 1) == 'o'
           && first(idx)     == 'n'
        then idx - 7
        else -1
      else -1

    /** The arity of a name ending in the suffix `Function{\d}`, but -1
     *  if the number is larger than Int.MaxValue / 10.
     *  @param suffixStart  The index of the suffix
     */
    private def funArity(suffixStart: Int): Int =
      inline val MaxSafeInt = MaxValue / 10
      val first = name.firstPart
      def collectDigits(acc: Int, idx: Int): Int =
        if idx == first.length then acc
        else
          val d = digit2int(first(idx), 10)
          if d < 0 || acc > MaxSafeInt then -1
          else collectDigits(acc * 10 + d, idx + 1)
      collectDigits(0, suffixStart + 8)

    private def isFunctionPrefix(suffixStart: Int, mustHave: String = "")(using Context): Boolean =
      suffixStart >= 0
      && {
        val first = name.firstPart
        var found = mustHave.isEmpty
        def skip(idx: Int, str: String) =
          if first.startsWith(str, idx) then
            if str == mustHave then found = true
            idx + str.length
          else idx
        skip(skip(0, "Impure"), "Context") == suffixStart
        && found
      }

    /** Same as `funArity`, except that it returns -1 if the prefix
     *  is not one of a (possibly empty) concatenation of a subset of
     *  "Impure" (only under pureFunctions), "Erased" and "Context" (in that order).
     */
    private def checkedFunArity(suffixStart: Int)(using Context): Int =
      if isFunctionPrefix(suffixStart) then funArity(suffixStart) else -1

    /** Is a function name, i.e one of FunctionXXL, FunctionN, ContextFunctionN, ImpureFunctionN, ImpureContextFunctionN for N >= 0
     */
    def isFunction(using Context): Boolean =
      (name eq tpnme.FunctionXXL)
      || checkedFunArity(functionSuffixStart) >= 0

    /** Is a function name
     *    - FunctionN for N >= 0
     */
    def isPlainFunction(using Context): Boolean = functionArity >= 0

    /** Is a function name that contains `mustHave` as a substring
     *  and has arity `minArity` or greater.
     */
    private def isSpecificFunction(mustHave: String, minArity: Int = 0)(using Context): Boolean =
      val suffixStart = functionSuffixStart
      isFunctionPrefix(suffixStart, mustHave) && funArity(suffixStart) >= minArity

    def isContextFunction(using Context): Boolean = isSpecificFunction("Context")
    def isImpureFunction(using Context): Boolean = isSpecificFunction("Impure")

    /** Is a synthetic function name, i.e. one of
     *    - FunctionN for N > 22
     *    - ContextFunctionN for N >= 0
     */
    def isSyntheticFunction(using Context): Boolean =
      val suffixStart = functionSuffixStart
      if suffixStart == 0 then funArity(suffixStart) > MaxImplementedFunctionArity
      else checkedFunArity(suffixStart) >= 0

    def functionArity(using Context): Int =
      val suffixStart = functionSuffixStart
      if suffixStart >= 0 then checkedFunArity(suffixStart) else -1

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

    /** This method is to be used on **type parameters** from a class, since
     *  this method does sorting based on their names
     */
    def specializedFor(classTargs: List[Type], classTargsNames: List[Name], methodTargs: List[Type], methodTarsNames: List[Name])(using Context): N = {
      val methodTags: Seq[Name] = (methodTargs zip methodTarsNames).sortBy(_._2).map(x => defn.typeTag(x._1))
      val classTags: Seq[Name] = (classTargs zip classTargsNames).sortBy(_._2).map(x => defn.typeTag(x._1))

      likeSpacedN(name ++ nme.specializedTypeNames.prefix ++
        methodTags.fold(nme.EMPTY)(_ ++ _) ++ nme.specializedTypeNames.separator ++
        classTags.fold(nme.EMPTY)(_ ++ _) ++ nme.specializedTypeNames.suffix)
    }

    /** Determines if the current name is the specialized name of the given base name.
     *  For example `typeName("Tuple2$mcII$sp").isSpecializedNameOf(tpnme.Tuple2) == true`
     */
    def isSpecializedNameOf(base: N)(using Context): Boolean =
      var i = 0
      inline def nextString(str: String) = name.startsWith(str, i) && { i += str.length; true }
      nextString(base.toString)
        && nextString(nme.specializedTypeNames.prefix.toString)
        && nextString(nme.specializedTypeNames.separator.toString)
        && name.endsWith(nme.specializedTypeNames.suffix.toString)

    /** Returns the name of the class specialised to the provided types,
     *  in the given order.  Used for the specialized tuple classes.
     */
    def specializedName(args: List[Type])(using Context): N =
      val sb = new StringBuilder
      sb.append(name.toString)
      sb.append(nme.specializedTypeNames.prefix.toString)
      sb.append(nme.specializedTypeNames.separator)
      args.foreach { arg => sb.append(defn.typeTag(arg)) }
      sb.append(nme.specializedTypeNames.suffix)
      likeSpacedN(termName(sb.toString))

    /** Use for specializing function names ONLY and use it if you are **not**
     *  creating specialized name from type parameters. The order of names will
     *  be:
     *
     *  `<return type><first type><second type><...>`
     */
    def specializedFunction(ret: Type, args: List[Type])(using Context): N =
      val sb = new StringBuilder
      sb.append(name.toString)
      sb.append(nme.specializedTypeNames.prefix.toString)
      sb.append(nme.specializedTypeNames.separator)
      sb.append(defn.typeTag(ret).toString)
      args.foreach { arg => sb.append(defn.typeTag(arg)) }
      sb.append(nme.specializedTypeNames.suffix)
      likeSpacedN(termName(sb.toString))

    /** If name length exceeds allowable limit, replace part of it by hash */
    def compactified(using Context): TermName = termName(compactify(name.toString))

    def unmangleClassName: N = name.toTermName match {
      case name: SimpleName
      if name.endsWith(str.MODULE_SUFFIX) && !nme.falseModuleClassNames.contains(name) =>
        likeSpacedN(name.dropRight(str.MODULE_SUFFIX.length).moduleClassName)
      case _ => name
    }

    def unmangle(kind: NameKind): N = likeSpacedN {
      name match
        case name: SimpleName =>
          kind.unmangle(name)
        case name: TypeName =>
          name.toTermName.unmangle(kind).toTypeName
        case _ =>
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
      val unmangled = kinds.foldLeft(name)(_.unmangle(_))
      if (unmangled eq name) name else unmangled.unmangle(kinds)
    }

    def firstCodePoint: Int =
      val first = name.firstPart
      import Character.{isHighSurrogate, isLowSurrogate, isValidCodePoint, toCodePoint}
      if isHighSurrogate(first(0)) && first.length > 1 && isLowSurrogate(first(1)) then
        val codepoint = toCodePoint(first(0), first(1))
        if isValidCodePoint(codepoint) then codepoint else first(0)
      else first(0)
  }

  extension (name: TermName) {

    def setterName: TermName = name.exclude(FieldName) ++ str.SETTER_SUFFIX

    def syntheticSetterName = SyntheticSetterName(name.exclude(FieldName))

    def getterName: TermName =
      val name1 = name.exclude(FieldName)
      if name1.is(SyntheticSetterName) then name1.exclude(SyntheticSetterName)
      else name1.mapLast(n =>
        if (n.endsWith(str.SETTER_SUFFIX)) n.take(n.length - str.SETTER_SUFFIX.length).asSimpleName
        else n)

    def fieldName: TermName =
      if (name.isSetterName)
        if name.is(SyntheticSetterName) then
          name.exclude(SyntheticSetterName)
            .replace { case TraitSetterName(_, original) => original }
            .fieldName
        else getterName.fieldName
      else FieldName(name.toSimpleName)

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

    /** If this is a super accessor name, its underlying name, which is the name
     *  of the method that the super accessor forwards to.
     */
    def originalOfSuperAccessorName: TermName = name match
      case SuperAccessorName(name1)   => name1.originalOfSuperAccessorName
      case ExpandedName(_, name1)     => name1.originalOfSuperAccessorName
      case ExpandPrefixName(_, name1) => name1.originalOfSuperAccessorName
      case _ => name
  }
}
