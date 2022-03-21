package dotty.tools
package dotc
package typer

import ast._
import core._
import Types._, ProtoTypes._, Contexts._, Decorators._, Denotations._, Symbols._
import Implicits._, Flags._, Constants.Constant
import Trees._
import NameOps._
import util.SrcPos
import config.Feature
import java.util.regex.Matcher.quoteReplacement
import reporting._
import collection.mutable

import scala.util.matching.Regex

object ErrorReporting {

  import tpd._

  def errorTree(tree: untpd.Tree, msg: Message, pos: SrcPos)(using Context): tpd.Tree =
    tree.withType(errorType(msg, pos))

  def errorTree(tree: untpd.Tree, msg: Message)(using Context): tpd.Tree =
    errorTree(tree, msg, tree.srcPos)

  def errorTree(tree: untpd.Tree, msg: TypeError, pos: SrcPos)(using Context): tpd.Tree =
    tree.withType(errorType(msg, pos))

  def errorType(msg: Message, pos: SrcPos)(using Context): ErrorType = {
    report.error(msg, pos)
    ErrorType(msg)
  }

  def errorType(ex: TypeError, pos: SrcPos)(using Context): ErrorType = {
    report.error(ex, pos)
    ErrorType(ex.toMessage)
  }

  def wrongNumberOfTypeArgs(fntpe: Type, expectedArgs: List[ParamInfo], actual: List[untpd.Tree], pos: SrcPos)(using Context): ErrorType =
    errorType(WrongNumberOfTypeArgs(fntpe, expectedArgs, actual), pos)

  def missingArgs(tree: Tree, mt: Type)(using Context): Unit =
    val meth = err.exprStr(methPart(tree))
    mt match
      case mt: MethodType if mt.paramNames.isEmpty =>
        report.error(MissingEmptyArgumentList(meth), tree.srcPos)
      case _ =>
        report.error(em"missing arguments for $meth", tree.srcPos)

  def matchReductionAddendum(tps: Type*)(using Context): String =
    val collectMatchTrace = new TypeAccumulator[String]:
      def apply(s: String, tp: Type): String =
        if s.nonEmpty then s
        else tp match
          case tp: AppliedType if tp.isMatchAlias => MatchTypeTrace.record(tp.tryNormalize)
          case tp: MatchType => MatchTypeTrace.record(tp.tryNormalize)
          case _ => foldOver(s, tp)
    tps.foldLeft("")(collectMatchTrace)

  class Errors(using Context) {

    /** An explanatory note to be added to error messages
     *  when there's a problem with abstract var defs */
    def abstractVarMessage(sym: Symbol): String =
      if (sym.underlyingSymbol.is(Mutable))
        "\n(Note that variables need to be initialized to be defined)"
      else ""

    def expectedTypeStr(tp: Type): String = tp match {
      case tp: PolyProto =>
        em"type arguments [${tp.targs.tpes}%, %] and ${expectedTypeStr(tp.resultType)}"
      case tp: FunProto =>
        val result = tp.resultType match {
          case _: WildcardType | _: IgnoredProto => ""
          case tp => em" and expected result type $tp"
        }
        em"arguments (${tp.typedArgs().tpes}%, %)$result"
      case _ =>
        em"expected type $tp"
    }

    def anonymousTypeMemberStr(tpe: Type): String = {
      val kind = tpe match {
        case _: TypeBounds => "type with bounds"
        case _: MethodOrPoly => "method"
        case _ => "value of type"
      }
      em"$kind $tpe"
    }

    def overloadedAltsStr(alts: List[SingleDenotation]): String =
      em"overloaded alternatives of ${denotStr(alts.head)} with types\n" +
      em" ${alts map (_.info)}%\n %"

    def denotStr(denot: Denotation): String =
      if (denot.isOverloaded) overloadedAltsStr(denot.alternatives)
      else if (denot.symbol.exists) denot.symbol.showLocated
      else anonymousTypeMemberStr(denot.info)

    def refStr(tp: Type): String = tp match {
      case tp: NamedType =>
        if tp.denot.symbol.exists then tp.denot.symbol.showLocated
        else
          val kind = tp.info match
            case _: MethodOrPoly | _: ExprType => "method"
            case _ => if tp.isType then "type" else "value"
          s"$kind ${tp.name}"
      case _ => anonymousTypeMemberStr(tp)
    }

    def exprStr(tree: Tree): String = refStr(tree.tpe)

    def takesNoParamsStr(tree: Tree, kind: String): String =
      if (tree.tpe.widen.exists)
        i"${exprStr(tree)} does not take ${kind}parameters"
      else {
        i"undefined: $tree # ${tree.uniqueId}: ${tree.tpe.toString} at ${ctx.phase}"
      }

    def patternConstrStr(tree: Tree): String = ???

    def typeMismatch(tree: Tree, pt: Type, implicitFailure: SearchFailureType = NoMatchingImplicits): Tree = {
      val normTp = normalize(tree.tpe, pt)
      val treeTp = if (normTp <:< pt) tree.tpe else normTp
        // use normalized type if that also shows an error, original type otherwise
      def missingElse = tree match
        case If(_, _, elsep @ Literal(Constant(()))) if elsep.span.isSynthetic =>
          "\nMaybe you are missing an else part for the conditional?"
        case _ => ""
      errorTree(tree, TypeMismatch(treeTp, pt, Some(tree), implicitFailure.whyNoConversion, missingElse))
    }

    /** A subtype log explaining why `found` does not conform to `expected` */
    def whyNoMatchStr(found: Type, expected: Type): String =
      val header =
        i"""I tried to show that
          |  $found
          |conforms to
          |  $expected
          |but the comparison trace ended with `false`:
          |"""
      val c = ctx.typerState.constraint
      val constraintText =
        if c.domainLambdas.isEmpty then
          "the empty constraint"
        else
          i"""a constraint with:
             |$c"""
      i"""${TypeComparer.explained(_.isSubType(found, expected), header)}
         |
         |The tests were made under $constraintText"""

    /** Format `raw` implicitNotFound or implicitAmbiguous argument, replacing
     *  all occurrences of `${X}` where `X` is in `paramNames` with the
     *  corresponding shown type in `args`.
     */

    def rewriteNotice: String =
      if Feature.migrateTo3 then "\nThis patch can be inserted automatically under -rewrite."
      else ""

    def whyFailedStr(fail: FailedExtension) =
      i"""    failed with
         |
         |${fail.whyFailed.message.indented(8)}"""

    def selectErrorAddendum
      (tree: untpd.RefTree, qual1: Tree, qualType: Type, suggestImports: Type => String, foundWithoutNull: Boolean = false)
      (using Context): String =

      val attempts = mutable.ListBuffer[(Tree, String)]()
      val nested = mutable.ListBuffer[NestedFailure]()
      for
        failures <- qual1.getAttachment(Typer.HiddenSearchFailure)
        failure <- failures
      do
        failure.reason match
          case fail: NestedFailure => nested += fail
          case fail: FailedExtension => attempts += ((failure.tree, whyFailedStr(fail)))
          case fail: Implicits.NoMatchingImplicits => // do nothing
          case _ => attempts += ((failure.tree, ""))
      if foundWithoutNull then
        i""".
          |Since explicit-nulls is enabled, the selection is rejected because
          |${qualType.widen} could be null at runtime.
          |If you want to select ${tree.name} without checking for a null value,
          |insert a .nn before .${tree.name} or import scala.language.unsafeNulls."""
      else if qualType.derivesFrom(defn.DynamicClass) then
        "\npossible cause: maybe a wrong Dynamic method signature?"
      else if attempts.nonEmpty then
        val attemptStrings =
          attempts.toList
            .map((tree, whyFailed) => (tree.showIndented(4), whyFailed))
            .distinctBy(_._1)
            .map((treeStr, whyFailed) =>
              i"""
                 |    $treeStr$whyFailed""")
        val extMethods =
          if attemptStrings.length > 1 then "Extension methods were"
          else "An extension method was"
        i""".
           |$extMethods tried, but could not be fully constructed:
           |$attemptStrings%\n%"""
      else if nested.nonEmpty then
        i""".
           |Extension methods were tried, but the search failed with:
           |
           |${nested.head.explanation.indented(4)}"""
      else if tree.hasAttachment(desugar.MultiLineInfix) then
        i""".
           |Note that `${tree.name}` is treated as an infix operator in Scala 3.
           |If you do not want that, insert a `;` or empty line in front
           |or drop any spaces behind the operator."""
      else if qualType.isExactlyNothing then
        ""
      else
        val add = suggestImports(
          ViewProto(qualType.widen,
            SelectionProto(tree.name, WildcardType, NoViewsAllowed, privateOK = false)))
        if add.isEmpty then ""
        else ", but could be made available as an extension method." ++ add
    end selectErrorAddendum
  }

  def substitutableTypeSymbolsInScope(sym: Symbol)(using Context): List[Symbol] =
    sym.ownersIterator.takeWhile(!_.is(Flags.Package)).flatMap { ownerSym =>
      ownerSym.paramSymss.flatten.filter(_.isType) ++
      ownerSym.typeRef.nonClassTypeMembers.map(_.symbol)
    }.toList

  def dependentStr =
    """Term-dependent types are experimental,
      |they must be enabled with a `experimental.dependent` language import or setting""".stripMargin

  def err(using Context): Errors = new Errors
}

class ImplicitSearchError(
  arg: tpd.Tree,
  pt: Type,
  where: String,
  paramSymWithMethodCallTree: Option[(Symbol, tpd.Tree)] = None,
  ignoredInstanceNormalImport: => Option[SearchSuccess],
  importSuggestionAddendum: => String
)(using ctx: Context) {

  def missingArgMsg = arg.tpe match {
    case ambi: AmbiguousImplicits =>
      (ambi.alt1, ambi.alt2) match {
        case (alt @ AmbiguousImplicitMsg(msg), _) =>
          userDefinedAmbiguousImplicitMsg(alt, msg)
        case (_, alt @ AmbiguousImplicitMsg(msg)) =>
          userDefinedAmbiguousImplicitMsg(alt, msg)
        case _ =>
          defaultAmbiguousImplicitMsg(ambi)
      }
    case _ =>
      val shortMessage = userDefinedImplicitNotFoundParamMessage
        .orElse(userDefinedImplicitNotFoundTypeMessage)
        .getOrElse(defaultImplicitNotFoundMessage)
      formatMsg(shortMessage)()
      ++ hiddenImplicitsAddendum
      ++ ErrorReporting.matchReductionAddendum(pt)
  }

  private def formatMsg(shortForm: String)(headline: String = shortForm) = arg match {
    case arg: Trees.SearchFailureIdent[?] =>
      shortForm
    case _ =>
      arg.tpe match {
        case tpe: SearchFailureType =>
          val original = arg match
            case Inlined(call, _, _) => call
            case _ => arg

          i"""$headline.
            |I found:
            |
            |    ${original.show.replace("\n", "\n    ")}
            |
            |But ${tpe.explanation}."""
      }
  }

  private def userDefinedErrorString(raw: String, paramNames: List[String], args: List[Type]): String = {
    def translate(name: String): Option[String] = {
      val idx = paramNames.indexOf(name)
      if (idx >= 0) Some(ex"${args(idx)}") else None
    }

    """\$\{\s*([^}\s]+)\s*\}""".r.replaceAllIn(raw, (_: Regex.Match) match {
      case Regex.Groups(v) => quoteReplacement(translate(v).getOrElse("")).nn
    })
  }

  /** Extract a user defined error message from a symbol `sym`
   *  with an annotation matching the given class symbol `cls`.
   */
  private def userDefinedMsg(sym: Symbol, cls: Symbol) = for {
    ann <- sym.getAnnotation(cls)
    msg <- ann.argumentConstantString(0)
  } yield msg

  private def location(preposition: String) = if (where.isEmpty) "" else s" $preposition $where"

  private def defaultAmbiguousImplicitMsg(ambi: AmbiguousImplicits) = {
    formatMsg(s"ambiguous implicit arguments: ${ambi.explanation}${location("of")}")(
      s"ambiguous implicit arguments of type ${pt.show} found${location("for")}"
    )
  }

  private def defaultImplicitNotFoundMessage = {
    ex"no implicit argument of type $pt was found${location("for")}"
  }

  /** Construct a custom error message given an ambiguous implicit
   *  candidate `alt` and a user defined message `raw`.
   */
  private def userDefinedAmbiguousImplicitMsg(alt: SearchSuccess, raw: String) = {
    val params = alt.ref.underlying match {
      case p: PolyType => p.paramNames.map(_.toString)
      case _           => Nil
    }
    def resolveTypes(targs: List[tpd.Tree])(using Context) =
      targs.map(a => Inferencing.fullyDefinedType(a.tpe, "type argument", a.span))

    // We can extract type arguments from:
    //   - a function call:
    //     @implicitAmbiguous("msg A=${A}")
    //     implicit def f[A](): String = ...
    //     implicitly[String] // found: f[Any]()
    //
    //   - an eta-expanded function:
    //     @implicitAmbiguous("msg A=${A}")
    //     implicit def f[A](x: Int): String = ...
    //     implicitly[Int => String] // found: x => f[Any](x)

    val call = tpd.closureBody(alt.tree) // the tree itself if not a closure
    val targs = tpd.typeArgss(call).flatten
    val args = resolveTypes(targs)(using ctx.fresh.setTyperState(alt.tstate))
    userDefinedErrorString(raw, params, args)
  }

  /** @param rawMsg           Message template with variables, e.g. "Variable A is ${A}"
   *  @param sym              Symbol of the annotated type or of the method whose parameter was annotated
   *  @param substituteType   Function substituting specific types for abstract types associated with variables, e.g A -> Int
   */
  private def formatAnnotationMessage(rawMsg: String, sym: Symbol, substituteType: Type => Type): String = {
    val substitutableTypesSymbols = ErrorReporting.substitutableTypeSymbolsInScope(sym)

    userDefinedErrorString(
      rawMsg,
      paramNames = substitutableTypesSymbols.map(_.name.unexpandedName.toString),
      args = substitutableTypesSymbols.map(_.typeRef).map(substituteType)
    )
  }

  /** Extracting the message from a method parameter, e.g. in
   *
   *  trait Foo
   *
   *  def foo(implicit @annotation.implicitNotFound("Foo is missing") foo: Foo): Any = ???
   */
  private def userDefinedImplicitNotFoundParamMessage: Option[String] = paramSymWithMethodCallTree.flatMap { (sym, applTree) =>
    userDefinedMsg(sym, defn.ImplicitNotFoundAnnot).map { rawMsg =>
      val fn = tpd.funPart(applTree)
      val targs = tpd.typeArgss(applTree).flatten
      val methodOwner = fn.symbol.owner
      val methodOwnerType = tpd.qualifier(fn).tpe
      val methodTypeParams = fn.symbol.paramSymss.flatten.filter(_.isType)
      val methodTypeArgs = targs.map(_.tpe)
      val substituteType = (_: Type).asSeenFrom(methodOwnerType, methodOwner).subst(methodTypeParams, methodTypeArgs)
      formatAnnotationMessage(rawMsg, sym.owner, substituteType)
    }
  }

  /** Extracting the message from a type, e.g. in
   *
   *  @annotation.implicitNotFound("Foo is missing")
   *  trait Foo
   *
   *  def foo(implicit foo: Foo): Any = ???
   */
  private def userDefinedImplicitNotFoundTypeMessage: Option[String] =
    def recur(tp: Type): Option[String] = tp match
      case tp: TypeRef =>
        val sym = tp.symbol
        userDefinedImplicitNotFoundTypeMessage(sym).orElse(recur(tp.info))
      case tp: ClassInfo =>
        tp.baseClasses.iterator
          .map(userDefinedImplicitNotFoundTypeMessage)
          .find(_.isDefined).flatten
      case tp: TypeProxy =>
        recur(tp.underlying)
      case tp: AndType =>
        recur(tp.tp1).orElse(recur(tp.tp2))
      case _ =>
        None
    recur(pt)

  private def userDefinedImplicitNotFoundTypeMessage(sym: Symbol): Option[String] =
    for
      rawMsg <- userDefinedMsg(sym, defn.ImplicitNotFoundAnnot)
      if Feature.migrateTo3 || sym != defn.Function1
        // Don't inherit "No implicit view available..." message if subtypes of Function1 are not treated as implicit conversions anymore
    yield
      val substituteType = (_: Type).asSeenFrom(pt, sym)
      formatAnnotationMessage(rawMsg, sym, substituteType)

  private def hiddenImplicitsAddendum: String =
    def hiddenImplicitNote(s: SearchSuccess) =
      em"\n\nNote: ${s.ref.symbol.showLocated} was not considered because it was not imported with `import given`."

    val normalImports = ignoredInstanceNormalImport.map(hiddenImplicitNote)

    normalImports.getOrElse(importSuggestionAddendum)
  end hiddenImplicitsAddendum

  private object AmbiguousImplicitMsg {
    def unapply(search: SearchSuccess): Option[String] =
      userDefinedMsg(search.ref.symbol, defn.ImplicitAmbiguousAnnot)
  }
}
