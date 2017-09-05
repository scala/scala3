package dotty.tools
package dotc
package reporting
package diagnostic

import dotc.core._
import Contexts.Context
import Decorators._
import Symbols._
import Names._
import NameOps._
import Types._
import util.SourcePosition
import config.Settings.Setting
import interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotc.parsing.Scanners.Token
import dotc.parsing.Tokens
import printing.Highlighting._
import printing.Formatting
import ErrorMessageID._
import Denotations.SingleDenotation
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.untpd.Modifiers
import dotty.tools.dotc.core.Flags.{FlagSet, Mutable}
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import scala.util.control.NonFatal

object messages {

  // `MessageContainer`s to be consumed by `Reporter` ---------------------- //
  class Error(
    msgFn: => Message,
    pos: SourcePosition
  ) extends MessageContainer(msgFn, pos, ERROR)

  class Warning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends MessageContainer(msgFn, pos, WARNING) {
    def toError: Error = new Error(msgFn, pos)
  }

  class Info(
    msgFn: => Message,
    pos: SourcePosition
  ) extends MessageContainer(msgFn, pos, INFO)

  abstract class ConditionalWarning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends Warning(msgFn, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean]
  }

  class FeatureWarning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.feature
  }

  class UncheckedWarning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.unchecked
  }

  class DeprecationWarning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.deprecation
  }

  class MigrationWarning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.migration
  }

  /**  Messages
    *  ========
    *  The role of messages is to provide the necessary details for a simple to
    *  understand diagnostic event. Each message can be turned into a message
    *  container (one of the above) by calling the appropriate method on them.
    *  For instance:
    *
    *  ```scala
    *  EmptyCatchBlock(tree).error(pos)   // res: Error
    *  EmptyCatchBlock(tree).warning(pos) // res: Warning
    *  ```
    */
  import ast.Trees._
  import ast.untpd
  import ast.tpd

  /** Helper methods for messages */
  def implicitClassRestrictionsText(implicit ctx: Context) =
    hl"""|${NoColor("For a full list of restrictions on implicit classes visit")}
         |${Blue("http://docs.scala-lang.org/overviews/core/implicit-classes.html")}"""


  // Syntax Errors ---------------------------------------------------------- //
  abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree, errNo: ErrorMessageID)(implicit ctx: Context)
  extends Message(EmptyCatchOrFinallyBlockID) {
    val explanation = {
      val tryString = tryBody match {
        case Block(Nil, untpd.EmptyTree) => "{}"
        case _ => tryBody.show
      }

      val code1 =
        s"""|import scala.util.control.NonFatal
            |
            |try $tryString catch {
            |  case NonFatal(e) => ???
            |}""".stripMargin

      val code2 =
        s"""|try $tryString finally {
            |  // perform your cleanup here!
            |}""".stripMargin

      hl"""|A ${"try"} expression should be followed by some mechanism to handle any exceptions
           |thrown. Typically a ${"catch"} expression follows the ${"try"} and pattern matches
           |on any expected exceptions. For example:
           |
           |$code1
           |
           |It is also possible to follow a ${"try"} immediately by a ${"finally"} - letting the
           |exception propagate - but still allowing for some clean up in ${"finally"}:
           |
           |$code2
           |
           |It is recommended to use the ${"NonFatal"} extractor to catch all exceptions as it
           |correctly handles transfer functions like ${"return"}."""
    }
  }

  case class EmptyCatchBlock(tryBody: untpd.Tree)(implicit ctx: Context)
 extends EmptyCatchOrFinallyBlock(tryBody, EmptyCatchBlockID) {
    val kind = "Syntax"
    val msg =
      hl"""|The ${"catch"} block does not contain a valid expression, try
           |adding a case like - `${"case e: Exception =>"}` to the block"""
  }

  case class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody, EmptyCatchAndFinallyBlockID) {
    val kind = "Syntax"
    val msg =
      hl"""|A ${"try"} without ${"catch"} or ${"finally"} is equivalent to putting
           |its body in a block; no exceptions are handled."""
  }

  case class DeprecatedWithOperator()(implicit ctx: Context)
  extends Message(DeprecatedWithOperatorID) {
    val kind = "Syntax"
    val msg =
      hl"""${"with"} as a type operator has been deprecated; use `&' instead"""
    val explanation =
      hl"""|Dotty introduces intersection types - `&' types. These replace the
           |use of the ${"with"} keyword. There are a few differences in
           |semantics between intersection types and using `${"with"}'."""
  }

  case class CaseClassMissingParamList(cdef: untpd.TypeDef)(implicit ctx: Context)
  extends Message(CaseClassMissingParamListID) {
    val kind = "Syntax"
    val msg =
      hl"""|A ${"case class"} must have at least one parameter list"""

    val explanation =
      hl"""|${cdef.name} must have at least one parameter list, if you would rather
           |have a singleton representation of ${cdef.name}, use a "${"case object"}".
           |Or, add an explicit `()' as a parameter list to ${cdef.name}."""
  }

  case class AnonymousFunctionMissingParamType(param: untpd.ValDef,
                                               args: List[untpd.Tree],
                                               tree: untpd.Function,
                                               pt: Type)
                                              (implicit ctx: Context)
  extends Message(AnonymousFunctionMissingParamTypeID) {
    val kind = "Syntax"

    val msg = {
      val ofFun =
        if (MethodType.syntheticParamNames(args.length + 1) contains param.name)
          i" of expanded function $tree"
        else
          ""

      i"missing parameter type for parameter ${param.name}$ofFun, expected = $pt"
    }

    val explanation =
      hl"""|Anonymous functions must define a type. For example, if you define a function like this one:
           |
           |${"val f = { case xs @ List(1, 2, 3) => Some(xs) }"}
           |
           |Make sure you give it a type of what you expect to match and help the type inference system:
           |
           |${"val f: Seq[Int] => Option[List[Int]] = { case xs @ List(1, 2, 3) => Some(xs) }"} """
  }

  case class WildcardOnTypeArgumentNotAllowedOnNew()(implicit ctx: Context)
  extends Message(WildcardOnTypeArgumentNotAllowedOnNewID) {
    val kind = "syntax"
    val msg = "type argument must be fully defined"

    val code1 =
      """
        |object TyperDemo {
        |  class Team[A]
        |  val team = new Team[_]
        |}
      """.stripMargin

    val code2 =
      """
        |object TyperDemo {
        |  class Team[A]
        |  val team = new Team[Int]
        |}
      """.stripMargin

    val explanation =
      hl"""|Wildcard on arguments is not allowed when declaring a new type.
           |
           |Given the following example:
           |
           |$code1
           |
           |You must complete all the type parameters, for instance:
           |
           |$code2 """
  }


  // Type Errors ------------------------------------------------------------ //
  case class DuplicateBind(bind: untpd.Bind, tree: untpd.CaseDef)(implicit ctx: Context)
  extends Message(DuplicateBindID) {
    val kind = "Naming"
    val msg = em"duplicate pattern variable: `${bind.name}`"

    val explanation = {
      val pat = tree.pat.show
      val guard = tree.guard match {
        case untpd.EmptyTree => ""
        case guard => s"if ${guard.show}"
      }

      val body = tree.body match {
        case Block(Nil, untpd.EmptyTree) => ""
        case body => s" ${body.show}"
      }

      val caseDef = s"case $pat$guard => $body"

      hl"""|For each ${"case"} bound variable names have to be unique. In:
           |
           |$caseDef
           |
           |`${bind.name}` is not unique. Rename one of the bound variables!"""
    }
  }

  case class MissingIdent(tree: untpd.Ident, treeKind: String, name: String)(implicit ctx: Context)
  extends Message(MissingIdentID) {
    val kind = "Unbound Identifier"
    val msg = em"not found: $treeKind$name"

    val explanation = {
      hl"""|The identifier for `$treeKind$name` is not bound, that is,
           |no declaration for this identifier can be found.
           |That can happen for instance if $name or its declaration has either been
           |misspelt, or if you're forgetting an import"""
    }
  }

  case class TypeMismatch(found: Type, expected: Type, whyNoMatch: String = "", implicitFailure: String = "")(implicit ctx: Context)
  extends Message(TypeMismatchID) {
    val kind = "Type Mismatch"
    val msg = {
      val (where, printCtx) = Formatting.disambiguateTypes(found, expected)
      val (fnd, exp) = Formatting.typeDiff(found, expected)(printCtx)
      s"""|found:    $fnd
          |required: $exp
          |
          |$where""".stripMargin + whyNoMatch + implicitFailure
    }

    val explanation = ""
  }

  case class NotAMember(site: Type, name: Name, selected: String)(implicit ctx: Context)
  extends Message(NotAMemberID) {
    val kind = "Member Not Found"

    //println(i"site = $site, decls = ${site.decls}, source = ${site.widen.typeSymbol.sourceFile}") //DEBUG

    val msg = {
      import core.Flags._
      val maxDist = 3
      val decls = site.decls.toList.flatMap { sym =>
        if (sym.flagsUNSAFE.is(Synthetic | PrivateOrLocal) || sym.isConstructor) Nil
        else List((sym.name.show, sym))
      }

      // Calculate Levenshtein distance
      def distance(n1: Iterable[_], n2: Iterable[_]) =
        n1.foldLeft(List.range(0, n2.size)) { (prev, x) =>
          (prev zip prev.tail zip n2).scanLeft(prev.head + 1) {
            case (h, ((d, v), y)) => math.min(
              math.min(h + 1, v + 1),
              if (x == y) d else d + 1
            )
          }
        }.last

      // Count number of wrong characters
      def incorrectChars(x: (String, Int, Symbol)): (String, Symbol, Int) = {
        val (currName, _, sym) = x
        val matching = name.show.zip(currName).foldLeft(0) {
          case (acc, (x,y)) => if (x != y) acc + 1 else acc
        }
        (currName, sym, matching)
      }

      // Get closest match in `site`
      val closest =
        decls
        .map { case (n, sym) => (n, distance(n, name.show), sym) }
        .collect { case (n, dist, sym) if dist <= maxDist => (n, dist, sym) }
        .groupBy(_._2).toList
        .sortBy(_._1)
        .headOption.map(_._2).getOrElse(Nil)
        .map(incorrectChars).toList
        .sortBy(_._3)
        .take(1).map { case (n, sym, _) => (n, sym) }

      val siteName = site match {
        case site: NamedType => site.name.show
        case site => i"$site"
      }

      val closeMember = closest match {
        case (n, sym) :: Nil => hl""" - did you mean `${s"$siteName.$n"}`?"""
        case Nil => ""
        case _ => assert(
          false,
          "Could not single out one distinct member to match on input with"
        )
      }

      val siteType = site match {
        case tp: NamedType => tp.info
        case tp => tp
      }

      ex"$selected `$name` is not a member of $siteType$closeMember"
    }

    val explanation = ""
  }

  case class EarlyDefinitionsNotSupported()(implicit ctx: Context)
  extends Message(EarlyDefinitionsNotSupportedID) {
    val kind = "Syntax"
    val msg = "early definitions are not supported; use trait parameters instead"

    val explanation = {
      val code1 =
        """|trait Logging {
           |  val f: File
           |  f.open()
           |  onExit(f.close())
           |  def log(msg: String) = f.write(msg)
           |}
           |
           |class B extends Logging {
           |  val f = new File("log.data") // triggers a NullPointerException
           |}
           |
           |// early definition gets around the NullPointerException
           |class C extends {
           |  val f = new File("log.data")
           |} with Logging""".stripMargin

      val code2 =
        """|trait Logging(f: File) {
           |  f.open()
           |  onExit(f.close())
           |  def log(msg: String) = f.write(msg)
           |}
           |
           |class C extends Logging(new File("log.data"))""".stripMargin

      hl"""|Earlier versions of Scala did not support trait parameters and "early
           |definitions" (also known as "early initializers") were used as an alternative.
           |
           |Example of old syntax:
           |
           |$code1
           |
           |The above code can now be written as:
           |
           |$code2
           |"""
    }
  }

  case class TopLevelImplicitClass(cdef: untpd.TypeDef)(implicit ctx: Context)
  extends Message(TopLevelImplicitClassID) {
    val kind = "Syntax"
    val msg = hl"""An ${"implicit class"} may not be top-level"""

    val explanation = {
      val TypeDef(name, impl @ Template(constr0, parents, self, _)) = cdef
      val exampleArgs =
        if(constr0.vparamss.isEmpty) "..."
        else constr0.vparamss(0).map(_.withMods(untpd.Modifiers()).show).mkString(", ")
      def defHasBody[T] = impl.body.exists(!_.isEmpty)
      val exampleBody = if (defHasBody) "{\n ...\n }" else ""
      hl"""|There may not be any method, member or object in scope with the same name as
           |the implicit class and a case class automatically gets a companion object with
           |the same name created by the compiler which would cause a naming conflict if it
           |were allowed.
           |
           |""" + implicitClassRestrictionsText + hl"""|
           |
           |To resolve the conflict declare ${cdef.name} inside of an ${"object"} then import the class
           |from the object at the use site if needed, for example:
           |
           |object Implicits {
           |  implicit class ${cdef.name}($exampleArgs)$exampleBody
           |}
           |
           |// At the use site:
           |import Implicits.${cdef.name}"""
    }
  }

  case class ImplicitCaseClass(cdef: untpd.TypeDef)(implicit ctx: Context)
  extends Message(ImplicitCaseClassID) {
    val kind = "Syntax"
    val msg = hl"""A ${"case class"} may not be defined as ${"implicit"}"""

    val explanation =
      hl"""|implicit classes may not be case classes. Instead use a plain class:
           |
           |implicit class ${cdef.name}...
           |
           |""" + implicitClassRestrictionsText
  }

  case class ObjectMayNotHaveSelfType(mdef: untpd.ModuleDef)(implicit ctx: Context)
  extends Message(ObjectMayNotHaveSelfTypeID) {
    val kind = "Syntax"
    val msg = hl"""${"object"}s must not have a self ${"type"}"""

    val explanation = {
      val untpd.ModuleDef(name, tmpl) = mdef
      val ValDef(_, selfTpt, _) = tmpl.self
      hl"""|${"object"}s must not have a self ${"type"}:
           |
           |Consider these alternative solutions:
           |  - Create a trait or a class instead of an object
           |  - Let the object extend a trait containing the self type:
           |
           |    object $name extends ${selfTpt.show}"""
    }
  }

  case class TupleTooLong(ts: List[untpd.Tree])(implicit ctx: Context)
  extends Message(TupleTooLongID) {
    import Definitions.MaxTupleArity
    val kind = "Syntax"
    val msg = hl"""A ${"tuple"} cannot have more than ${MaxTupleArity} members"""

    val explanation = {
      val members = ts.map(_.showSummary).grouped(MaxTupleArity)
      val nestedRepresentation = members.map(_.mkString(", ")).mkString(")(")
      hl"""|This restriction will be removed in the future.
           |Currently it is possible to use nested tuples when more than $MaxTupleArity are needed, for example:
           |
           |((${nestedRepresentation}))"""
    }
  }

  case class RepeatedModifier(modifier: String)(implicit ctx:Context)
  extends Message(RepeatedModifierID) {
    val kind = "Syntax"
    val msg = hl"""repeated modifier $modifier"""

    val explanation = {
      val code1 = hl"""private private val Origin = Point(0, 0)"""
      val code2 = hl"""private final val Origin = Point(0, 0)"""
      hl"""This happens when you accidentally specify the same modifier twice.
           |
           |Example:
           |
           |$code1
           |
           |instead of
           |
           |$code2
           |
           |"""
    }
  }

  case class InterpolatedStringError()(implicit ctx:Context)
  extends Message(InterpolatedStringErrorID) {
    val kind = "Syntax"
    val msg = "error in interpolated string: identifier or block expected"
    val explanation = {
      val code1 = "s\"$new Point(0, 0)\""
      val code2 = "s\"${new Point(0, 0)}\""
      hl"""|This usually happens when you forget to place your expressions inside curly braces.
           |
           |$code1
           |
           |should be written as
           |
           |$code2
           |"""
    }
  }

  case class UnboundPlaceholderParameter()(implicit ctx:Context)
  extends Message(UnboundPlaceholderParameterID) {
    val kind = "Syntax"
    val msg = "unbound placeholder parameter; incorrect use of `_`"
    val explanation =
      hl"""|The `_` placeholder syntax was used where it could not be bound.
           |Consider explicitly writing the variable binding.
           |
           |This can be done by replacing `_` with a variable (eg. `x`)
           |and adding ${"x =>"} where applicable.
           |
           |Example before:
           |
           |${"{ _ }"}
           |
           |Example after:
           |
           |${"x => { x }"}
           |
           |Another common occurrence for this error is defining a val with `_`:
           |
           |${"val a = _"}
           |
           |But this val definition isn't very useful, it can never be assigned
           |another value. And thus will always remain uninitialized.
           |Consider replacing the ${"val"} with ${"var"}:
           |
           |${"var a = _"}
           |
           |Note that this use of `_` is not placeholder syntax,
           |but an uninitialized var definition.
           |Only fields can be left uninitialized in this manner; local variables
           |must be initialized.
           |"""
  }

  case class IllegalStartSimpleExpr(illegalToken: String)(implicit ctx: Context)
  extends Message(IllegalStartSimpleExprID) {
    val kind = "Syntax"
    val msg = "illegal start of simple expression"
    val explanation = {
      hl"""|An expression yields a value. In the case of the simple expression, this error
           |commonly occurs when there's a missing parenthesis or brace. The reason being
           |that a simple expression is one of the following:
           |
           |- Block
           |- Expression in parenthesis
           |- Identifier
           |- Object creation
           |- Literal
           |
           |which cannot start with ${Red(illegalToken)}."""
    }
  }

  case class MissingReturnType()(implicit ctx:Context)
  extends Message(MissingReturnTypeID) {
    val kind = "Syntax"
    val msg = "missing return type"
    val explanation =
      hl"""|An abstract declaration must have a return type. For example:
           |
           |trait Shape {
           |  def area: Double // abstract declaration returning a ${"Double"}
           |}"""
  }

  case class YieldOrDoExpectedInForComprehension()(implicit ctx: Context)
  extends Message(YieldOrDoExpectedInForComprehensionID) {
    val kind = "Syntax"
    val msg = hl"${"yield"} or ${"do"} expected"

    val explanation =
      hl"""|When the enumerators in a for comprehension are not placed in parentheses or
           |braces, a ${"do"} or ${"yield"} statement is required after the enumerators
           |section of the comprehension.
           |
           |You can save some keystrokes by omitting the parentheses and writing
           |
           |${"val numbers = for i <- 1 to 3 yield i"}
           |
           |  instead of
           |
           |${"val numbers = for (i <- 1 to 3) yield i"}
           |
           |but the ${"yield"} keyword is still required.
           |
           |For comprehensions that simply perform a side effect without yielding anything
           |can also be written without parentheses but a ${"do"} keyword has to be
           |included. For example,
           |
           |${"for (i <- 1 to 3) println(i)"}
           |
           |can be written as
           |
           |${"for i <- 1 to 3 do println(i) // notice the 'do' keyword"}
           |
           |"""
  }

  case class ProperDefinitionNotFound()(implicit ctx: Context)
  extends Message(ProperDefinitionNotFoundID) {
    val kind = "Definition Not Found"
    val msg = hl"""Proper definition was not found in ${"@usecase"}"""

    val explanation = {
      val noUsecase =
        "def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That"

      val usecase =
        """|/** Map from List[A] => List[B]
           |  *
           |  * @usecase def map[B](f: A => B): List[B]
           |  */
           |def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That
           |""".stripMargin

      hl"""|Usecases are only supported for ${"def"}s. They exist because with Scala's
           |advanced type-system, we sometimes end up with seemingly scary signatures.
           |The usage of these methods, however, needs not be - for instance the `map`
           |function
           |
           |${"List(1, 2, 3).map(2 * _) // res: List(2, 4, 6)"}
           |
           |is easy to understand and use - but has a rather bulky signature:
           |
           |$noUsecase
           |
           |to mitigate this and ease the usage of such functions we have the ${"@usecase"}
           |annotation for docstrings. Which can be used like this:
           |
           |$usecase
           |
           |When creating the docs, the signature of the method is substituted by the
           |usecase and the compiler makes sure that it is valid. Because of this, you're
           |only allowed to use ${"def"}s when defining usecases."""
    }
  }

  case class ByNameParameterNotSupported()(implicit ctx: Context)
  extends Message(ByNameParameterNotSupportedID) {
    val kind = "Syntax"
    val msg = "By-name parameter type not allowed here."

    val explanation =
      hl"""|By-name parameters act like functions that are only evaluated when referenced,
           |allowing for lazy evaluation of a parameter.
           |
           |An example of using a by-name parameter would look like:
           |${"def func(f: => Boolean) = f // 'f' is evaluated when referenced within the function"}
           |
           |An example of the syntax of passing an actual function as a parameter:
           |${"def func(f: (Boolean => Boolean)) = f(true)"}
           |
           |or:
           |
           |${"def func(f: Boolean => Boolean) = f(true)"}
           |
           |And the usage could be as such:
           |${"func(bool => // do something...)"}
           |"""
  }

  case class WrongNumberOfTypeArgs(fntpe: Type, expectedArgs: List[ParamInfo], actual: List[untpd.Tree])(implicit ctx: Context)
  extends Message(WrongNumberOfTypeArgsID) {
    val kind = "Syntax"

    private val expectedCount = expectedArgs.length
    private val actualCount = actual.length
    private val msgPrefix = if (actualCount > expectedCount) "Too many" else "Not enough"

    //TODO add def simpleParamName to ParamInfo
    private val expectedArgString = expectedArgs
      .map(_.paramName.unexpandedName.show)
      .mkString("[", ", ", "]")

    private val actualArgString = actual.map(_.show).mkString("[", ", ", "]")

    private val prettyName =
      try
        fntpe.termSymbol match {
          case NoSymbol => fntpe.show
          case symbol   => symbol.showFullName
        }
      catch {
        case NonFatal(ex) => fntpe.show
      }

    val msg =
      hl"""|${NoColor(msgPrefix)} type arguments for $prettyName$expectedArgString
           |expected: $expectedArgString
           |actual:   $actualArgString""".stripMargin

    val explanation = {
      val tooManyTypeParams =
        """|val tuple2: (Int, String) = (1, "one")
           |val list: List[(Int, String)] = List(tuple2)""".stripMargin

      if (actualCount > expectedCount)
        hl"""|You have supplied too many type parameters
             |
             |For example List takes a single type parameter (List[A])
             |If you need to hold more types in a list then you need to combine them
             |into another data type that can contain the number of types you need,
             |In this example one solution would be to use a Tuple:
             |
             |${tooManyTypeParams}"""
      else
        hl"""|You have not supplied enough type parameters
             |If you specify one type parameter then you need to specify every type parameter."""
    }
  }

  case class IllegalVariableInPatternAlternative()(implicit ctx: Context)
  extends Message(IllegalVariableInPatternAlternativeID) {
    val kind = "Syntax"
    val msg = "Variables are not allowed in alternative patterns"
    val explanation = {
      val varInAlternative =
        """|def g(pair: (Int,Int)): Int = pair match {
           |  case (1, n) | (n, 1) => n
           |  case _ => 0
           |}""".stripMargin

      val fixedVarInAlternative =
        """|def g(pair: (Int,Int)): Int = pair match {
           |  case (1, n) => n
           |  case (n, 1) => n
           |  case _ => 0
           |}""".stripMargin

      hl"""|Variables are not allowed within alternate pattern matches. You can workaround
           |this issue by adding additional cases for each alternative. For example, the
           |illegal function:
           |
           |$varInAlternative
           |could be implemented by moving each alternative into a separate case:
           |
           |$fixedVarInAlternative"""
    }
  }

  case class IdentifierExpected(identifier: String)(implicit ctx: Context)
  extends Message(IdentifierExpectedID) {
    val kind = "Syntax"
    val msg = "identifier expected"
    val explanation = {
      val wrongIdentifier = s"def foo: $identifier = {...}"
      val validIdentifier = s"def foo = {...}"
      hl"""|An identifier expected, but `$identifier` found. This could be because
           |`$identifier` is not a valid identifier. As a workaround, the compiler could
           |infer the type for you. For example, instead of:
           |
           |$wrongIdentifier
           |
           |Write your code like:
           |
           |$validIdentifier
           |
           |"""
    }
  }

  case class AuxConstructorNeedsNonImplicitParameter()(implicit ctx:Context)
  extends Message(AuxConstructorNeedsNonImplicitParameterID) {
    val kind = "Syntax"
    val msg = "auxiliary constructor needs non-implicit parameter list"
    val explanation =
      hl"""|Only the primary constructor is allowed an ${"implicit"} parameter list;
           |auxiliary constructors need non-implicit parameter lists. When a primary
           |constructor has an implicit argslist, auxiliary constructors that call the
           |primary constructor must specify the implicit value.
           |
           |To resolve this issue check for:
           | - forgotten parenthesis on ${"this"} (${"def this() = { ... }"})
           | - auxiliary constructors specify the implicit value
           |"""
  }

  case class IncorrectRepeatedParameterSyntax()(implicit ctx: Context)
  extends Message(IncorrectRepeatedParameterSyntaxID) {
    val kind = "Syntax"
    val msg = "'*' expected"
    val explanation =
      hl"""|Expected * in '_*' operator.
           |
           |The '_*' operator can be used to supply a sequence-based argument
           |to a method with a variable-length or repeated parameter. It is used
           |to expand the sequence to a variable number of arguments, such that:
           |func(args: _*) would expand to func(arg1, arg2 ... argN).
           |
           |Below is an example of how a method with a variable-length
           |parameter can be declared and used.
           |
           |Squares the arguments of a variable-length parameter:
           |${"def square(args: Int*) = args.map(a => a * a)"}
           |
           |Usage:
           |${"square(1, 2, 3) // res0: List[Int] = List(1, 4, 9)"}
           |
           |Secondary Usage with '_*':
           |${"val ints = List(2, 3, 4)  // ints: List[Int] = List(2, 3, 4)"}
           |${"square(ints: _*)          // res1: List[Int] = List(4, 9, 16)"}
           |""".stripMargin
  }

  case class IllegalLiteral()(implicit ctx: Context)
  extends Message(IllegalLiteralID) {
    val kind = "Syntax"
    val msg = "illegal literal"
    val explanation =
      hl"""|Available literals can be divided into several groups:
           | - Integer literals: 0, 21, 0xFFFFFFFF, -42L
           | - Floating Point Literals: 0.0, 1e30f, 3.14159f, 1.0e-100, .1
           | - Boolean Literals: true, false
           | - Character Literals: 'a', '\u0041', '\n'
           | - String Literals: "Hello, World!"
           | - null
           |"""
  }

  case class PatternMatchExhaustivity(uncovered: String)(implicit ctx: Context)
  extends Message(PatternMatchExhaustivityID) {
    val kind = "Pattern Match Exhaustivity"
    val msg =
      hl"""|match may not be exhaustive.
           |
           |It would fail on: $uncovered"""


    val explanation =
      hl"""|There are several ways to make the match exhaustive:
           | - Add missing cases as shown in the warning
           | - If an extractor always return 'Some(...)', write 'Some[X]' for its return type
           | - Add a 'case _ => ...' at the end to match all remaining cases
           |"""
  }

  case class MatchCaseUnreachable()(implicit ctx: Context)
  extends Message(MatchCaseUnreachableID) {
    val kind = s"""Match ${hl"case"} Unreachable"""
    val msg = "unreachable code"
    val explanation = ""
  }

  case class SeqWildcardPatternPos()(implicit ctx: Context)
  extends Message(SeqWildcardPatternPosID) {
    val kind = "Syntax"
    val msg = "`_*' can be used only for last argument"
    val explanation = {
      val code =
        """def sumOfTheFirstTwo(list: List[Int]): Int = list match {
          |  case List(first, second, x:_*) => first + second
          |  case _ => 0
          |}"""
      hl"""|Sequence wildcard pattern is expected at the end of an argument list.
           |This pattern matches any remaining elements in a sequence.
           |Consider the following example:
           |
           |$code
           |
           |Calling:
           |
           |${"sumOfTheFirstTwo(List(1, 2, 10))"}
           |
           |would give 3 as a result"""
    }
  }

  case class IllegalStartOfSimplePattern()(implicit ctx: Context)
  extends Message(IllegalStartOfSimplePatternID) {
    val kind = "Syntax"
    val msg = "illegal start of simple pattern"
    val explanation = {
      val sipCode =
        """def f(x: Int, y: Int) = x match {
          |  case `y` => ...
          |}
        """
      val constructorPatternsCode =
        """case class Person(name: String, age: Int)
          |
          |def test(p: Person) = p match {
          |  case Person(name, age) => ...
          |}
        """
      val tupplePatternsCode =
        """def swap(tuple: (String, Int)): (Int, String) = tuple match {
          |  case (text, number) => (number, text)
          |}
        """
      val patternSequencesCode =
        """def getSecondValue(list: List[Int]): Int = list match {
          |  case List(_, second, x:_*) => second
          |  case _ => 0
          |}"""
      hl"""|Simple patterns can be divided into several groups:
           |- Variable Patterns: ${"case x => ..."}.
           |  It matches any value, and binds the variable name to that value.
           |  A special case is the wild-card pattern _ which is treated as if it was a fresh
           |  variable on each occurrence.
           |
           |- Typed Patterns: ${"case x: Int => ..."} or ${"case _: Int => ..."}.
           |  This pattern matches any value matched by the specified type; it binds the variable
           |  name to that value.
           |
           |- Literal Patterns: ${"case 123 => ..."} or ${"case 'A' => ..."}.
           |  This type of pattern matches any value that is equal to the specified literal.
           |
           |- Stable Identifier Patterns:
           |
           |  $sipCode
           |
           |  the match succeeds only if the x argument and the y argument of f are equal.
           |
           |- Constructor Patterns:
           |
           |  $constructorPatternsCode
           |
           |  The pattern binds all object's fields to the variable names (name and age, in this
           |  case).
           |
           |- Tuple Patterns:
           |
           |  $tupplePatternsCode
           |
           |  Calling:
           |
           |  ${"""swap(("Luftballons", 99)"""}
           |
           |  would give ${"""(99, "Luftballons")"""} as a result.
           |
           |- Pattern Sequences:
           |
           |  $patternSequencesCode
           |
           |  Calling:
           |
           |  ${"getSecondValue(List(1, 10, 2))"}
           |
           |  would give 10 as a result.
           |  This pattern is possible because a companion object for the List class has a method
           |  with the following signature:
           |
           |  ${"def unapplySeq[A](x: List[A]): Some[List[A]]"}
           |"""
    }
  }

  case class PkgDuplicateSymbol(existing: Symbol)(implicit ctx: Context)
  extends Message(PkgDuplicateSymbolID) {
    val kind = "Duplicate Symbol"
    val msg = hl"trying to define package with same name as `$existing`"
    val explanation = ""
  }

  case class ExistentialTypesNoLongerSupported()(implicit ctx: Context)
  extends Message(ExistentialTypesNoLongerSupportedID) {
    val kind = "Syntax"
    val msg =
      hl"""|Existential types are no longer supported -
           |use a wildcard or dependent type instead"""
    val explanation =
      hl"""|The use of existential types is no longer supported.
           |
           |You should use a wildcard or dependent type instead.
           |
           |For example:
           |
           |Instead of using ${"forSome"} to specify a type variable
           |
           |${"List[T forSome { type T }]"}
           |
           |Try using a wildcard type variable
           |
           |${"List[_]"}
           |"""
  }

  case class UnboundWildcardType()(implicit ctx: Context)
  extends Message(UnboundWildcardTypeID) {
    val kind = "Syntax"
    val msg = "Unbound wildcard type"
    val explanation =
      hl"""|The wildcard type syntax (`_`) was used where it could not be bound.
           |Replace `_` with a non-wildcard type. If the type doesn't matter,
           |try replacing `_` with ${"Any"}.
           |
           |Examples:
           |
           |- Parameter lists
           |
           |  Instead of:
           |    ${"def foo(x: _) = ..."}
           |
           |  Use ${"Any"} if the type doesn't matter:
           |    ${"def foo(x: Any) = ..."}
           |
           |- Type arguments
           |
           |  Instead of:
           |    ${"val foo = List[_](1, 2)"}
           |
           |  Use:
           |    ${"val foo = List[Int](1, 2)"}
           |
           |- Type bounds
           |
           |  Instead of:
           |    ${"def foo[T <: _](x: T) = ..."}
           |
           |  Remove the bounds if the type doesn't matter:
           |    ${"def foo[T](x: T) = ..."}
           |
           |- ${"val"} and ${"def"} types
           |
           |  Instead of:
           |    ${"val foo: _ = 3"}
           |
           |  Use:
           |    ${"val foo: Int = 3"}
           |"""
  }

  case class DanglingThisInPath()(implicit ctx: Context) extends Message(DanglingThisInPathID) {
    val kind = "Syntax"
    val msg = hl"""Expected an additional member selection after the keyword ${"this"}"""

    val contextCode =
      """  trait Outer {
        |    val member: Int
        |    type Member
        |    trait Inner {
        |      ...
        |    }
        |  }"""

    val importCode =
      """  import Outer.this.member
        |  //               ^^^^^^^"""

    val typeCode =
      """  type T = Outer.this.Member
        |  //                 ^^^^^^^"""

    val explanation =
      hl"""|Paths of imports and type selections must not end with the keyword ${"this"}.
           |
           |Maybe you forgot to select a member of ${"this"}? As an example, in the
           |following context:
           |${contextCode}
           |
           |- this is a valid import expression using a path
           |${importCode}
           |
           |- this is a valid type using a path
           |${typeCode}
           |"""
  }

  case class OverridesNothing(member: Symbol)(implicit ctx: Context)
  extends Message(OverridesNothingID) {
    val kind = "Reference"
    val msg = hl"""${member} overrides nothing"""

    val explanation =
      hl"""|There must be a field or method with the name `${member.name}` in a super
           |class of `${member.owner}` to override it. Did you misspell it?
           |Are you extending the right classes?
           |"""
  }

  case class OverridesNothingButNameExists(member: Symbol, existing: List[Denotations.SingleDenotation])(implicit ctx: Context)
  extends Message(OverridesNothingButNameExistsID) {
    val kind = "Reference"
    val msg = hl"""${member} has a different signature than the overridden declaration"""

    val existingDecl = existing.map(_.showDcl).mkString("  \n")

    val explanation =
      hl"""|There must be a non-final field or method with the name `${member.name}` and the
           |same parameter list in a super class of `${member.owner}` to override it.
           |
           |  ${member.showDcl}
           |
           |The super classes of `${member.owner}` contain the following members
           |named `${member.name}`:
           |  ${existingDecl}
           |"""
  }

  case class ForwardReferenceExtendsOverDefinition(value: Symbol, definition: Symbol)(implicit ctx: Context)
  extends Message(ForwardReferenceExtendsOverDefinitionID) {
    val kind = "Reference"
    val msg = hl"`${definition.name}` is a forward reference extending over the definition of `${value.name}`"

    val explanation =
      hl"""|`${definition.name}` is used before you define it, and the definition of `${value.name}`
           |appears between that use and the definition of `${definition.name}`.
           |
           |Forward references are allowed only, if there are no value definitions between
           |the reference and the referred method definition.
           |
           |Define `${definition.name}` before it is used,
           |or move the definition of `${value.name}` so it does not appear between
           |the declaration of `${definition.name}` and its use,
           |or define `${value.name}` as lazy.
           |""".stripMargin
  }

  case class ExpectedTokenButFound(expected: Token, found: Token)(implicit ctx: Context)
  extends Message(ExpectedTokenButFoundID) {
    val kind = "Syntax"

    private val expectedText =
      if (Tokens.isIdentifier(expected)) "an identifier"
      else Tokens.showToken(expected)

    private val foundText = Tokens.showToken(found)

    val msg = hl"""${expectedText} expected, but ${foundText} found"""

    private val ifKeyword =
      if (Tokens.isIdentifier(expected) && Tokens.isKeyword(found))
        s"""
           |If you necessarily want to use $foundText as identifier, you may put it in backticks.""".stripMargin
      else
        ""
    val explanation = s"$ifKeyword"
  }

  case class MixedLeftAndRightAssociativeOps(op1: Name, op2: Name, op2LeftAssoc: Boolean)(implicit ctx: Context)
  extends Message(MixedLeftAndRightAssociativeOpsID) {
    val kind = "Syntax"
    val op1Asso = if (op2LeftAssoc) "which is right-associative" else "which is left-associative"
    val op2Asso = if (op2LeftAssoc) "which is left-associative" else "which is right-associative"
    val msg = s"`${op1}` (${op1Asso}) and `${op2}` ($op2Asso) have same precedence and may not be mixed"
    val explanation =
      s"""|The operators ${op1} and ${op2} are used as infix operators in the same expression,
          |but they bind to different sides:
          |${op1} is applied to the operand to its ${if (op2LeftAssoc) "right" else "left"}
          |${op2} is applied to the operand to its ${if (op2LeftAssoc) "left" else "right"}
          |As both have the same precedence the compiler can't decide which to apply first.
          |
          |You may use parenthesis to make the application order explicit,
          |or use method application syntax `operand1.${op1}(operand2)`.
          |
          |Operators ending in a colon `:` are right-associative. All other operators are left-associative.
          |
          |Infix operator precedence is determined by the operator's first character. Characters are listed
          |below in increasing order of precedence, with characters on the same line having the same precedence.
          |  (all letters)
          |  |
          |  ^
          |  &
          |  = !
          |  < >
          |  :
          |  + -
          |  * / %
          |  (all other special characters)
          |Operators starting with a letter have lowest precedence, followed by operators starting with `|`, etc.
          |""".stripMargin
  }

  case class CantInstantiateAbstractClassOrTrait(cls: Symbol, isTrait: Boolean)(implicit ctx: Context)
  extends Message(CantInstantiateAbstractClassOrTraitID) {
    val kind = "Usage"
    private val traitOrAbstract = if (isTrait) hl"a trait" else hl"abstract"
    val msg = hl"""${cls.name} is ${traitOrAbstract}; it cannot be instantiated"""
    val explanation =
      hl"""|Abstract classes and traits need to be extended by a concrete class or object
           |to make their functionality accessible.
           |
           |You may want to create an anonymous class extending ${cls.name} with
           |  ${s"class ${cls.name} { }"}
           |
           |or add a companion object with
           |  ${s"object ${cls.name} extends ${cls.name}"}
           |
           |You need to implement any abstract members in both cases.
           |""".stripMargin
  }

  case class OverloadedOrRecursiveMethodNeedsResultType private (termName: String)(implicit ctx: Context)
  extends Message(OverloadedOrRecursiveMethodNeedsResultTypeID) {
    val kind = "Syntax"
    val msg = hl"""overloaded or recursive method $termName needs return type"""
    val explanation =
      hl"""Case 1: $termName is overloaded
          |If there are multiple methods named `$termName` and at least one definition of
          |it calls another, you need to specify the calling method's return type.
          |
          |Case 2: $termName is recursive
          |If `$termName` calls itself on any path, you need to specify its return type.
          |""".stripMargin
  }

  object OverloadedOrRecursiveMethodNeedsResultType {
    def apply[T >: Trees.Untyped](tree: NameTree[T])(implicit ctx: Context)
    : OverloadedOrRecursiveMethodNeedsResultType =
      OverloadedOrRecursiveMethodNeedsResultType(tree.name.show)(ctx)
    def apply(symbol: Symbol)(implicit ctx: Context)
    : OverloadedOrRecursiveMethodNeedsResultType =
      OverloadedOrRecursiveMethodNeedsResultType(symbol.name.show)(ctx)
  }

  case class RecursiveValueNeedsResultType(tree: Names.TermName)(implicit ctx: Context)
  extends Message(RecursiveValueNeedsResultTypeID) {
    val kind = "Syntax"
    val msg = hl"""recursive value ${tree.name} needs type"""
    val explanation =
      hl"""The definition of `${tree.name}` is recursive and you need to specify its type.
          |""".stripMargin
  }

  case class CyclicReferenceInvolving(denot: SymDenotation)(implicit ctx: Context)
  extends Message(CyclicReferenceInvolvingID) {
    val kind = "Syntax"
    val msg = hl"""cyclic reference involving $denot"""
    val explanation =
      hl"""|$denot is declared as part of a cycle which makes it impossible for the
           |compiler to decide upon ${denot.name}'s type.
           |""".stripMargin
  }

  case class CyclicReferenceInvolvingImplicit(cycleSym: Symbol)(implicit ctx: Context)
  extends Message(CyclicReferenceInvolvingImplicitID) {
    val kind = "Syntax"
    val msg = hl"""cyclic reference involving implicit $cycleSym"""
    val explanation =
      hl"""|This happens when the right hand-side of $cycleSym's definition involves an implicit search.
           |To avoid this error, give `${cycleSym.name}` an explicit type.
           |""".stripMargin
  }

  case class SuperQualMustBeParent(qual: untpd.Ident, cls: Symbols.ClassSymbol)(implicit ctx: Context)
  extends Message(SuperQualMustBeParentID) {

    val msg = hl"""|$qual does not name a parent of $cls"""
    val kind = "Reference"

    private val parents: Seq[String] = (cls.info.parents map (_.name.show)).sorted

    val explanation =
      hl"""|When a qualifier ${"T"} is used in a ${"super"} prefix of the form ${"C.super[T]"},
           |${"T"} must be a parent type of ${"C"}.
           |
           |In this case, the parents of $cls are:
           |${parents.mkString("  - ", "\n  - ", "")}
           |""".stripMargin
  }

  case class VarArgsParamMustComeLast()(implicit ctx: Context)
  extends Message(IncorrectRepeatedParameterSyntaxID) {
    val msg = "varargs parameter must come last"
    val kind = "Syntax"
    val explanation =
      hl"""|The varargs field must be the last field in the method signature.
           |Attempting to define a field in a method signature after a varargs field is an error.
           |"""
  }

  case class AmbiguousImport(name: Names.Name, newPrec: Int, prevPrec: Int, prevCtx: Context)(implicit ctx: Context)
    extends Message(AmbiguousImportID) {

    import typer.Typer.BindingPrec

    /** A string which explains how something was bound; Depending on `prec` this is either
      *      imported by <tree>
      *  or  defined in <symbol>
      */
    private def bindingString(prec: Int, whereFound: Context, qualifier: String = "") = {
      val howVisible = prec match {
        case BindingPrec.definition => "defined"
        case BindingPrec.namedImport => "imported by name"
        case BindingPrec.wildImport => "imported"
        case BindingPrec.packageClause => "found"
      }
      if (BindingPrec.isImportPrec(prec)) {
        ex"""$howVisible$qualifier by ${hl"${whereFound.importInfo}"}"""
      } else
        ex"""$howVisible$qualifier in ${hl"${whereFound.owner}"}"""
    }


    val msg =
      i"""|reference to `${hl"$name"}` is ambiguous
          |it is both ${bindingString(newPrec, ctx)}
          |and ${bindingString(prevPrec, prevCtx, " subsequently")}"""

    val kind = "Reference"

    val explanation =
      hl"""|The compiler can't decide which of the possible choices you
           |are referencing with $name.
           |Note:
           |- Definitions take precedence over imports
           |- Named imports take precedence over wildcard imports
           |- You may replace a name when imported using
           |  ${"import"} scala.{ $name => ${name.show + "Tick"} }
           |"""
  }

  case class MethodDoesNotTakeParameters(tree: tpd.Tree, methPartType: Types.Type)
  (err: typer.ErrorReporting.Errors)(implicit ctx: Context)
  extends Message(MethodDoesNotTakeParametersId) {
    private val more = tree match {
      case Apply(_, _) => " more"
      case _ => ""
    }

    val msg = hl"${err.refStr(methPartType)} does not take$more parameters"

    val kind = "Reference"

    private val noParameters = if (methPartType.widenSingleton.isInstanceOf[ExprType])
      hl"""|As ${err.refStr(methPartType)} is defined without parenthesis, you may
           |not use any at call-site, either.
           |"""
    else
      ""

    val explanation =
      s"""|You have specified more parameter lists as defined in the method definition(s).
          |$noParameters""".stripMargin

  }

  case class AmbiguousOverload(tree: tpd.Tree, alts: List[SingleDenotation], pt: Type)(
    err: typer.ErrorReporting.Errors)(
    implicit ctx: Context)
  extends Message(AmbiguousOverloadID) {

    private val all = if (alts.length == 2) "both" else "all"
    val msg =
      s"""|Ambiguous overload. The ${err.overloadedAltsStr(alts)}
          |$all match ${err.expectedTypeStr(pt)}""".stripMargin
    val kind = "Reference"
    val explanation =
      hl"""|There are ${alts.length} methods that could be referenced as the compiler knows too little
           |about the expected type.
           |You may specify the expected type e.g. by
           |- assigning it to a value with a specified type, or
           |- adding a type ascription as in `${"instance.myMethod: String => Int"}`
           |"""
  }

  case class ReassignmentToVal(name: Names.Name)(implicit ctx: Context)
    extends Message(ReassignmentToValID) {
    val kind = "Reference"
    val msg = hl"""reassignment to val `$name`"""
    val explanation =
      hl"""|You can not assign a new value to `$name` as values can't be changed.
           |Keep in mind that every statement has a value, so you may e.g. use
           |  ${"val"} $name ${"= if (condition) 2 else 5"}
           |In case you need a reassignable name, you can declare it as
           |variable
           |  ${"var"} $name ${"="} ...
           |""".stripMargin
  }

  case class TypeDoesNotTakeParameters(tpe: Types.Type, params: List[Trees.Tree[Trees.Untyped]])(implicit ctx: Context)
    extends Message(TypeDoesNotTakeParametersID) {
    val kind = "Reference"
    val msg = hl"$tpe does not take type parameters"

    private val ps =
      if (params.size == 1) hl"a type parameter ${params.head}"
      else hl"type parameters ${params.map(_.show).mkString(", ")}"

    val explanation =
      i"""You specified $ps for ${hl"$tpe"}, which is not
         |declared to take any.
         |"""
  }

  case class ParameterizedTypeLacksArguments(psym: Symbol)(implicit ctx: Context)
    extends Message(ParameterizedTypeLacksArgumentsID) {
    val msg = hl"parameterized $psym lacks argument list"
    val kind = "Reference"
    val explanation =
      hl"""The $psym is declared with non-implicit parameters, you may not leave
          |out the parameter list when extending it.
          |"""
  }

  case class VarValParametersMayNotBeCallByName(name: Names.TermName, mutable: Boolean)(implicit ctx: Context)
    extends Message(VarValParametersMayNotBeCallByNameID) {
    val msg = s"${if (mutable) "`var'" else "`val'"} parameters may not be call-by-name"
    val kind = "Syntax"
    val explanation =
      hl"""${"var"} and ${"val"} parameters of classes and traits may no be call-by-name. In case you
          |want the parameter to be evaluated on demand, consider making it just a parameter
          |and a ${"def"} in the class such as
          |  ${s"class MyClass(${name}Tick: => String) {"}
          |  ${s"  def $name() = ${name}Tick"}
          |  ${"}"}
          |"""
  }

  case class MissingTypeParameterFor(tpe: Type)(implicit ctx: Context)
    extends Message(MissingTypeParameterForID) {
    val msg = hl"missing type parameter for ${tpe}"
    val kind = "Syntax"
    val explanation = ""
  }

  case class DoesNotConformToBound(tpe: Type, which: String, bound: Type)(
    err: typer.ErrorReporting.Errors)(implicit ctx: Context)
    extends Message(DoesNotConformToBoundID) {
    val msg = hl"Type argument ${tpe} does not conform to $which bound $bound ${err.whyNoMatchStr(tpe, bound)}"
    val kind = "Type Mismatch"
    val explanation = ""
  }

  case class DoesNotConformToSelfType(category: String, selfType: Type, cls: Symbol,
                                      otherSelf: Type, relation: String, other: Symbol)(
    implicit ctx: Context)
    extends Message(DoesNotConformToSelfTypeID) {
    val msg = hl"""$category: self type $selfType of $cls does not conform to self type $otherSelf
                  |of $relation $other"""
    val kind = "Type Mismatch"
    val explanation =
      hl"""You mixed in $other which requires self type $otherSelf, but $cls has self type
          |$selfType and does not inherit from $otherSelf.
          |
          |Note: Self types are indicated with the notation
          |  ${s"class "}$other ${"{ this: "}$otherSelf${" => "}
        """
  }

  case class DoesNotConformToSelfTypeCantBeInstantiated(tp: Type, selfType: Type)(
    implicit ctx: Context)
    extends Message(DoesNotConformToSelfTypeCantBeInstantiatedID) {
    val msg = hl"""$tp does not conform to its self type $selfType; cannot be instantiated"""
    val kind = "Type Mismatch"
    val explanation =
      hl"""To create an instance of $tp it needs to inherit $selfType in some way.
          |
          |Note: Self types are indicated with the notation
          |  ${s"class "}$tp ${"{ this: "}$selfType${" => "}
          |"""
  }

  case class AbstractMemberMayNotHaveModifier(sym: Symbol, flag: FlagSet)(
    implicit ctx: Context)
    extends Message(AbstractMemberMayNotHaveModifierID) {
    val msg = hl"""${"abstract"} $sym may not have `$flag' modifier"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class TopLevelCantBeImplicit(sym: Symbol)(
    implicit ctx: Context)
    extends Message(TopLevelCantBeImplicitID) {
    val msg = hl"""${"implicit"} modifier cannot be used for top-level definitions"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class TypesAndTraitsCantBeImplicit(sym: Symbol)(
    implicit ctx: Context)
    extends Message(TypesAndTraitsCantBeImplicitID) {
    val msg = hl"""${"implicit"} modifier cannot be used for types or traits"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class OnlyClassesCanBeAbstract(sym: Symbol)(
    implicit ctx: Context)
    extends Message(OnlyClassesCanBeAbstractID) {
    val msg = hl"""${"abstract"} modifier can be used only for classes; it should be omitted for abstract members"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class AbstractOverrideOnlyInTraits(sym: Symbol)(
    implicit ctx: Context)
    extends Message(AbstractOverrideOnlyInTraitsID) {
    val msg = hl"""${"abstract override"} modifier only allowed for members of traits"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class TraitsMayNotBeFinal(sym: Symbol)(
    implicit ctx: Context)
    extends Message(TraitsMayNotBeFinalID) {
    val msg = hl"""$sym may not be ${"final"}"""
    val kind = "Syntax"
    val explanation =
      "A trait can never be final since it is abstract and must be extended to be useful."
  }

  case class NativeMembersMayNotHaveImplementation(sym: Symbol)(
    implicit ctx: Context)
    extends Message(NativeMembersMayNotHaveImplementationID) {
    val msg = hl"""${"@native"} members may not have an implementation"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class OnlyClassesCanHaveDeclaredButUndefinedMembers(sym: Symbol)(
    implicit ctx: Context)
    extends Message(OnlyClassesCanHaveDeclaredButUndefinedMembersID) {

    private val varNote =
      if (sym.is(Mutable)) "Note that variables need to be initialized to be defined."
      else ""
    val msg = hl"""only classes can have declared but undefined members"""
    val kind = "Syntax"
    val explanation = s"$varNote"
  }

  case class CannotExtendAnyVal(sym: Symbol)(implicit ctx: Context)
    extends Message(CannotExtendAnyValID) {
    val msg = hl"""$sym cannot extend ${"AnyVal"}"""
    val kind = "Syntax"
    val explanation =
      hl"""Only classes (not traits) are allowed to extend ${"AnyVal"}, but traits may extend
          |${"Any"} to become ${Green("\"universal traits\"")} which may only have ${"def"} members.
          |Universal traits can be mixed into classes that extend ${"AnyVal"}.
          |"""
  }

  case class CannotHaveSameNameAs(sym: Symbol, cls: Symbol, reason: CannotHaveSameNameAs.Reason)(implicit ctx: Context)
    extends Message(CannotHaveSameNameAsID) {
    import CannotHaveSameNameAs._
    def reasonMessage: String = reason match {
      case CannotBeOverridden => "class definitions cannot be overridden"
      case DefinedInSelf(self) =>
        s"""cannot define ${sym.showKind} member with the same name as a ${cls.showKind} member in self reference ${self.name}.
           |(Note: this can be resolved by using another name)
           |""".stripMargin
    }

    val msg = hl"""$sym cannot have the same name as ${cls.showLocated} -- """ + reasonMessage
    val kind = "Syntax"
    val explanation = ""
  }
  object CannotHaveSameNameAs {
    sealed trait Reason
    case object CannotBeOverridden extends Reason
    case class DefinedInSelf(self: tpd.ValDef) extends Reason
  }

  case class ValueClassesMayNotDefineInner(valueClass: Symbol, inner: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotDefineInnerID) {
    val msg = hl"""value classes may not define an inner class"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class ValueClassesMayNotDefineNonParameterField(valueClass: Symbol, field: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotDefineNonParameterFieldID) {
    val msg = hl"""value classes may not define non-parameter field"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class ValueClassesMayNotDefineASecondaryConstructor(valueClass: Symbol, constructor: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotDefineASecondaryConstructorID) {
    val msg = hl"""value classes may not define a secondary constructor"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class ValueClassesMayNotContainInitalization(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotContainInitalizationID) {
    val msg = hl"""value classes may not contain initialization statements"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class ValueClassesMayNotBeAbstract(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotBeAbstractID) {
    val msg = hl"""value classes may not be ${"abstract"}"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class ValueClassesMayNotBeContainted(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotBeContaintedID) {
    private val localOrMember = if (valueClass.owner.isTerm) "local class" else "member of another class"
    val msg = s"""value classes may not be a $localOrMember"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class ValueClassesMayNotWrapItself(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotWrapItselfID) {
    val msg = """a value class may not wrap itself"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class ValueClassParameterMayNotBeAVar(valueClass: Symbol, param: Symbol)(implicit ctx: Context)
    extends Message(ValueClassParameterMayNotBeAVarID) {
    val msg = hl"""a value class parameter may not be a ${"var"}"""
    val kind = "Syntax"
    val explanation =
      hl"""A value class must have exactly one ${"val"} parameter.
          |"""
  }

  case class ValueClassNeedsOneValParam(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassNeedsExactlyOneValParamID) {
    val msg = hl"""value class needs one ${"val"} parameter"""
    val kind = "Syntax"
    val explanation = ""
  }

  case class OnlyCaseClassOrCaseObjectAllowed()(implicit ctx: Context)
    extends Message(OnlyCaseClassOrCaseObjectAllowedID) {
    val msg = "only `case class` or `case object` allowed"
    val kind = "Syntax"
    val explanation = ""
  }

  case class ExpectedClassOrObjectDef()(implicit ctx: Context)
    extends Message(ExpectedClassOrObjectDefID) {
    val kind = "Syntax"
    val msg = "expected class or object definition"
    val explanation = ""
  }

  case class SuperCallsNotAllowedInline(symbol: Symbol)(implicit ctx: Context)
    extends Message(SuperCallsNotAllowedInlineID) {
    val kind = "Syntax"
    val msg = s"super call not allowed in inline $symbol"
    val explanation = "Method inlining prohibits calling superclass methods, as it may lead to confusion about which super is being called."
  }

  case class ModifiersNotAllowed(flags: FlagSet, printableType: Option[String])(implicit ctx: Context)
    extends Message(ModifiersNotAllowedID) {
    val kind = "Syntax"
    val msg = s"modifier(s) `$flags' not allowed for ${printableType.getOrElse("combination")}"
    val explanation = {
      val first = "sealed def y: Int = 1"
      val second = "sealed lazy class z"
      hl"""You tried to use a modifier that is inapplicable for the type of item under modification
         |
         |  Please see the official Scala Language Specification section on modifiers:
         |  https://www.scala-lang.org/files/archive/spec/2.11/05-classes-and-objects.html#modifiers
         |
         |Consider the following example:
         |$first
         |In this instance, the modifier 'sealed' is not applicable to the item type 'def' (method)
         |$second
         |In this instance, the modifier combination is not supported
        """
    }
  }

  case class ImplicitFunctionTypeNeedsNonEmptyParameterList()(implicit ctx: Context)
    extends Message(ImplicitFunctionTypeNeedsNonEmptyParameterListID) {
    val kind = "Syntax"
    val msg = "implicit function type needs non-empty parameter list"
    val explanation = {
      val code1 = "type Transactional[T] = implicit Transaction => T"
      val code2 = "val cl: implicit A => B"
      hl"""It is not allowed to leave implicit function parameter list empty.
         |Possible ways to define implicit function type:
         |
         |$code1
         |
         |or
         |
         |$code2""".stripMargin
    }
  }

  case class WrongNumberOfParameters(expected: Int)(implicit ctx: Context)
    extends Message(WrongNumberOfParametersID) {
    val kind = "Syntax"
    val msg = s"wrong number of parameters, expected: $expected"
    val explanation = ""
  }

  case class DuplicatePrivateProtectedQualifier()(implicit ctx: Context)
    extends Message(DuplicatePrivateProtectedQualifierID) {
    val kind = "Syntax"
    val msg = "duplicate private/protected qualifier"
    val explanation =
      hl"It is not allowed to combine `private` and `protected` modifiers even if they are qualified to different scopes"
  }

  case class ExpectedStartOfTopLevelDefinition()(implicit ctx: Context)
    extends Message(ExpectedStartOfTopLevelDefinitionID) {
    val kind = "Syntax"
    val msg = "expected start of definition"
    val explanation =
      hl"you have to provide either ${"class"}, ${"trait"}, ${"object"}, or ${"enum"} definitions after qualifiers"
  }

}
