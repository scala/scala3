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
import dotty.tools.dotc.config.ScalaVersion
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.typer.ErrorReporting.Errors
import scala.util.control.NonFatal
import StdNames.nme

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
    def enablingOption(implicit ctx: Context): Setting[Boolean] = ctx.settings.feature
  }

  class UncheckedWarning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean] = ctx.settings.unchecked
  }

  class DeprecationWarning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean] = ctx.settings.deprecation
  }

  class MigrationWarning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean] = ctx.settings.migration
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
  def implicitClassRestrictionsText(implicit ctx: Context): String =
    hl"""|${NoColor("For a full list of restrictions on implicit classes visit")}
         |${Blue("http://docs.scala-lang.org/overviews/core/implicit-classes.html")}"""


  // Syntax Errors ---------------------------------------------------------- //
  abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree, errNo: ErrorMessageID)(implicit ctx: Context)
  extends Message(EmptyCatchOrFinallyBlockID) {
    val explanation: String = {
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
    val kind: String = "Syntax"
    val msg: String =
      hl"""|The ${"catch"} block does not contain a valid expression, try
           |adding a case like - ${"case e: Exception =>"} to the block"""
  }

  case class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody, EmptyCatchAndFinallyBlockID) {
    val kind: String = "Syntax"
    val msg: String =
      hl"""|A ${"try"} without ${"catch"} or ${"finally"} is equivalent to putting
           |its body in a block; no exceptions are handled."""
  }

  case class DeprecatedWithOperator()(implicit ctx: Context)
  extends Message(DeprecatedWithOperatorID) {
    val kind: String = "Syntax"
    val msg: String =
      hl"""${"with"} as a type operator has been deprecated; use ${"&"} instead"""
    val explanation: String =
      hl"""|Dotty introduces intersection types - ${"&"} types. These replace the
           |use of the ${"with"} keyword. There are a few differences in
           |semantics between intersection types and using ${"with"}."""
  }

  case class CaseClassMissingParamList(cdef: untpd.TypeDef)(implicit ctx: Context)
  extends Message(CaseClassMissingParamListID) {
    val kind: String = "Syntax"
    val msg: String =
      hl"""|A ${"case class"} must have at least one parameter list"""

    val explanation: String =
      hl"""|${cdef.name} must have at least one parameter list, if you would rather
           |have a singleton representation of ${cdef.name}, use a "${"case object"}".
           |Or, add an explicit ${"()"} as a parameter list to ${cdef.name}."""
  }

  case class AnonymousFunctionMissingParamType(param: untpd.ValDef,
                                               args: List[untpd.Tree],
                                               tree: untpd.Function,
                                               pt: Type)
                                              (implicit ctx: Context)
  extends Message(AnonymousFunctionMissingParamTypeID) {
    val kind: String = "Syntax"

    val msg: String = {
      val ofFun =
        if (MethodType.syntheticParamNames(args.length + 1) contains param.name)
          i" of expanded function:\n$tree"
        else
          ""

      val inferred =
        if (pt == WildcardType) ""
        else i"\nWhat I could infer was: $pt"

      i"""Missing parameter type
         |
         |I could not infer the type of the parameter ${param.name}$ofFun.$inferred"""
    }

    val explanation: String = ""
  }

  case class WildcardOnTypeArgumentNotAllowedOnNew()(implicit ctx: Context)
  extends Message(WildcardOnTypeArgumentNotAllowedOnNewID) {
    val kind: String = "Syntax"
    val msg: String = "Type argument must be fully defined"

    val code1: String =
      """
        |object TyperDemo {
        |  class Team[A]
        |  val team = new Team[_]
        |}
      """.stripMargin

    val code2: String =
      """
        |object TyperDemo {
        |  class Team[A]
        |  val team = new Team[Int]
        |}
      """.stripMargin

    val explanation: String =
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
    val kind: String = "Naming"
    val msg: String = em"duplicate pattern variable: ${bind.name}"

    val explanation: String = {
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
           |${bind.name} is not unique. Rename one of the bound variables!"""
    }
  }

  case class MissingIdent(tree: untpd.Ident, treeKind: String, name: String)(implicit ctx: Context)
  extends Message(MissingIdentID) {
    val kind: String = "Unbound Identifier"
    val msg: String = em"Not found: $treeKind$name"

    val explanation: String = {
      hl"""|The identifier for `$treeKind$name` is not bound, that is,
           |no declaration for this identifier can be found.
           |That can happen, for example, if `$name` or its declaration has either been
           |misspelt or if an import is missing."""
    }
  }

  case class TypeMismatch(found: Type, expected: Type, whyNoMatch: String = "", implicitFailure: String = "")(implicit ctx: Context)
  extends Message(TypeMismatchID) {
    val kind: String = "Type Mismatch"
    val msg: String = {
      val (where, printCtx) = Formatting.disambiguateTypes(found, expected)
      val whereSuffix = if (where.isEmpty) where else s"\n\n$where"
      val (fnd, exp) = Formatting.typeDiff(found, expected)(printCtx)
      s"""|Found:    $fnd
          |Required: $exp""".stripMargin + whereSuffix + whyNoMatch + implicitFailure
    }

    val explanation: String = ""
  }

  case class NotAMember(site: Type, name: Name, selected: String, addendum: String = "")(implicit ctx: Context)
  extends Message(NotAMemberID) {
    val kind: String = "Member Not Found"

    //println(i"site = $site, decls = ${site.decls}, source = ${site.widen.typeSymbol.sourceFile}") //DEBUG

    val msg: String = {
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
        case (n, sym) :: Nil => s" - did you mean $siteName.$n?"
        case Nil => ""
        case _ => assert(
          false,
          "Could not single out one distinct member to match on input with"
        )
      }

      ex"$selected $name is not a member of ${site.widen}$closeMember$addendum"
    }

    val explanation: String = ""
  }

  case class EarlyDefinitionsNotSupported()(implicit ctx: Context)
  extends Message(EarlyDefinitionsNotSupportedID) {
    val kind: String = "Syntax"
    val msg: String = "Early definitions are not supported; use trait parameters instead"

    val explanation: String = {
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
    val kind: String = "Syntax"
    val msg: String = hl"""An ${"implicit class"} may not be top-level"""

    val explanation: String = {
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
    val kind: String = "Syntax"
    val msg: String = hl"""A ${"case class"} may not be defined as ${"implicit"}"""

    val explanation: String =
      hl"""|Implicit classes may not be case classes. Instead use a plain class:
           |
           |implicit class ${cdef.name}...
           |
           |""" + implicitClassRestrictionsText
  }

  case class ImplicitClassPrimaryConstructorArity()(implicit ctx: Context)
  extends Message(ImplicitClassPrimaryConstructorArityID){
    val kind: String = "Syntax"
    val msg: String = "Implicit classes must accept exactly one primary constructor parameter"
    val explanation: String = {
      val example = "implicit class RichDate(date: java.util.Date)"
      hl"""Implicit classes may only take one non-implicit argument in their constructor. For example:
          |
          | $example
          |
          |While it’s possible to create an implicit class with more than one non-implicit argument,
          |such classes aren’t used during implicit lookup.
          |""" + implicitClassRestrictionsText
    }
  }

  case class ObjectMayNotHaveSelfType(mdef: untpd.ModuleDef)(implicit ctx: Context)
  extends Message(ObjectMayNotHaveSelfTypeID) {
    val kind: String = "Syntax"
    val msg: String = hl"""${"object"}s must not have a self ${"type"}"""

    val explanation: String = {
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

  case class RepeatedModifier(modifier: String)(implicit ctx:Context)
  extends Message(RepeatedModifierID) {
    val kind: String = "Syntax"
    val msg: String = hl"""Repeated modifier $modifier"""

    val explanation: String = {
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
    val kind: String = "Syntax"
    val msg: String = "Error in interpolated string: identifier or block expected"
    val explanation: String = {
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
    val kind: String = "Syntax"
    val msg: String = hl"""Unbound placeholder parameter; incorrect use of ${"_"}"""
    val explanation: String =
      hl"""|The ${"_"} placeholder syntax was used where it could not be bound.
           |Consider explicitly writing the variable binding.
           |
           |This can be done by replacing ${"_"} with a variable (eg. ${"x"})
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
           |Another common occurrence for this error is defining a val with ${"_"}:
           |
           |${"val a = _"}
           |
           |But this val definition isn't very useful, it can never be assigned
           |another value. And thus will always remain uninitialized.
           |Consider replacing the ${"val"} with ${"var"}:
           |
           |${"var a = _"}
           |
           |Note that this use of ${"_"} is not placeholder syntax,
           |but an uninitialized var definition.
           |Only fields can be left uninitialized in this manner; local variables
           |must be initialized.
           |"""
  }

  case class IllegalStartSimpleExpr(illegalToken: String)(implicit ctx: Context)
  extends Message(IllegalStartSimpleExprID) {
    val kind: String = "Syntax"
    val msg: String = "expression expected"
    val explanation: String = {
      hl"""|An expression cannot start with ${Red(illegalToken)}."""
    }
  }

  case class MissingReturnType()(implicit ctx:Context)
  extends Message(MissingReturnTypeID) {
    val kind: String = "Syntax"
    val msg: String = "Missing return type"
    val explanation: String =
      hl"""|An abstract declaration must have a return type. For example:
           |
           |trait Shape {
           |  def area: Double // abstract declaration returning a ${"Double"}
           |}"""
  }

  case class MissingReturnTypeWithReturnStatement(method: Symbol)(implicit ctx: Context)
  extends Message(MissingReturnTypeWithReturnStatementID) {
    val kind: String = "Syntax"
    val msg: String = hl"$method has a return statement; it needs a result type"
    val explanation: String =
      hl"""|If a method contains a ${"return"} statement, it must have an
           |explicit return type. For example:
           |
           |${"def good: Int /* explicit return type */ = return 1"}"""
  }

  case class YieldOrDoExpectedInForComprehension()(implicit ctx: Context)
  extends Message(YieldOrDoExpectedInForComprehensionID) {
    val kind: String = "Syntax"
    val msg: String = hl"${"yield"} or ${"do"} expected"

    val explanation: String =
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
    val kind: String = "Definition Not Found"
    val msg: String = hl"""Proper definition was not found in ${"@usecase"}"""

    val explanation: String = {
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
           |The usage of these methods, however, needs not be - for instance the ${"map"}
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

  case class ByNameParameterNotSupported(tpe: untpd.TypTree)(implicit ctx: Context)
  extends Message(ByNameParameterNotSupportedID) {
    val kind: String = "Syntax"
    val msg: String = hl"By-name parameter type ${tpe} not allowed here."

    val explanation: String =
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
    val kind: String = "Syntax"

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

    val msg: String =
      hl"""|${NoColor(msgPrefix)} type arguments for $prettyName$expectedArgString
           |expected: $expectedArgString
           |actual:   ${NoColor(actualArgString)}""".stripMargin

    val explanation: String = {
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
    val kind: String = "Syntax"
    val msg: String = "Variables are not allowed in alternative patterns"
    val explanation: String = {
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
    val kind: String = "Syntax"
    val msg: String = "identifier expected"
    val explanation: String = {
      val wrongIdentifier = hl"def foo: $identifier = {...}"
      val validIdentifier = hl"def foo = {...}"
      hl"""|An identifier expected, but $identifier found. This could be because
           |$identifier is not a valid identifier. As a workaround, the compiler could
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
    val kind: String = "Syntax"
    val msg: String = "Auxiliary constructor needs non-implicit parameter list"
    val explanation: String =
      hl"""|Only the primary constructor is allowed an ${"implicit"} parameter list;
           |auxiliary constructors need non-implicit parameter lists. When a primary
           |constructor has an implicit argslist, auxiliary constructors that call the
           |primary constructor must specify the implicit value.
           |
           |To resolve this issue check for:
           | - Forgotten parenthesis on ${"this"} (${"def this() = { ... }"})
           | - Auxiliary constructors specify the implicit value
           |"""
  }

  case class IncorrectRepeatedParameterSyntax()(implicit ctx: Context)
  extends Message(IncorrectRepeatedParameterSyntaxID) {
    val kind: String = "Syntax"
    val msg: String = "'*' expected"
    val explanation: String =
      hl"""|Expected * in ${"_*"} operator.
           |
           |The ${"_*"} operator can be used to supply a sequence-based argument
           |to a method with a variable-length or repeated parameter. It is used
           |to expand the sequence to a variable number of arguments, such that:
           |${"func(args: _*)"} would expand to ${"func(arg1, arg2 ... argN)"}.
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
           |Secondary Usage with ${"_*"}:
           |${"val ints = List(2, 3, 4)  // ints: List[Int] = List(2, 3, 4)"}
           |${"square(ints: _*)          // res1: List[Int] = List(4, 9, 16)"}
           |""".stripMargin
  }

  case class IllegalLiteral()(implicit ctx: Context)
  extends Message(IllegalLiteralID) {
    val kind: String = "Syntax"
    val msg: String = "Illegal literal"
    val explanation: String =
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
    val kind: String = "Pattern Match Exhaustivity"
    val msg: String =
      hl"""|${"match"} may not be exhaustive.
           |
           |It would fail on pattern case: $uncovered"""


    val explanation: String =
      hl"""|There are several ways to make the match exhaustive:
           | - Add missing cases as shown in the warning
           | - If an extractor always return ${"Some(...)"}, write ${"Some[X]"} for its return type
           | - Add a ${"case _ => ..."} at the end to match all remaining cases
           |"""
  }

  case class UncheckedTypePattern(msg: String)(implicit ctx: Context)
    extends Message(UncheckedTypePatternID) {
    val kind: String = "Pattern Match Exhaustivity"

    val explanation: String =
      hl"""|Type arguments and type refinements are erased during compile time, thus it's
           |impossible to check them at run-time.
           |
           |You can either replace the type arguments by ${"_"} or use `@unchecked`.
           |"""
  }

  case class MatchCaseUnreachable()(implicit ctx: Context)
  extends Message(MatchCaseUnreachableID) {
    val kind: String = "Match case Unreachable"
    val msg: String = "Unreachable case"
    val explanation: String = ""
  }

  case class MatchCaseOnlyNullWarning()(implicit ctx: Context)
  extends Message(MatchCaseOnlyNullWarningID) {
    val kind: String = "Only null matched"
    val msg: String = hl"""Only ${"null"} is matched. Consider using ${"case null =>"} instead."""
    val explanation: String = ""
  }

  case class SeqWildcardPatternPos()(implicit ctx: Context)
  extends Message(SeqWildcardPatternPosID) {
    val kind: String = "Syntax"
    val msg: String = hl"""${"_*"} can be used only for last argument"""
    val explanation: String = {
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
    val kind: String = "Syntax"
    val msg: String = "pattern expected"
    val explanation: String = {
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
    val kind: String = "Duplicate Symbol"
    val msg: String = hl"Trying to define package with same name as $existing"
    val explanation: String = ""
  }

  case class ExistentialTypesNoLongerSupported()(implicit ctx: Context)
  extends Message(ExistentialTypesNoLongerSupportedID) {
    val kind: String = "Syntax"
    val msg: String =
      hl"""|Existential types are no longer supported -
           |use a wildcard or dependent type instead"""
    val explanation: String =
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
    val kind: String = "Syntax"
    val msg: String = "Unbound wildcard type"
    val explanation: String =
      hl"""|The wildcard type syntax (${"_"}) was used where it could not be bound.
           |Replace ${"_"} with a non-wildcard type. If the type doesn't matter,
           |try replacing ${"_"} with ${"Any"}.
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
    val kind: String = "Syntax"
    val msg: String = hl"""Expected an additional member selection after the keyword ${"this"}"""

    val contextCode: String =
      """  trait Outer {
        |    val member: Int
        |    type Member
        |    trait Inner {
        |      ...
        |    }
        |  }"""

    val importCode: String =
      """  import Outer.this.member
        |  //               ^^^^^^^"""

    val typeCode: String =
      """  type T = Outer.this.Member
        |  //                 ^^^^^^^"""

    val explanation: String =
      hl"""|Paths of imports and type selections must not end with the keyword ${"this"}.
           |
           |Maybe you forgot to select a member of ${"this"}? As an example, in the
           |following context:
           |${contextCode}
           |
           |- This is a valid import expression using a path
           |${importCode}
           |
           |- This is a valid type using a path
           |${typeCode}
           |"""
  }

  case class OverridesNothing(member: Symbol)(implicit ctx: Context)
  extends Message(OverridesNothingID) {
    val kind: String = "Reference"
    val msg: String = hl"""${member} overrides nothing"""

    val explanation: String =
      hl"""|There must be a field or method with the name ${member.name} in a super
           |class of ${member.owner} to override it. Did you misspell it?
           |Are you extending the right classes?
           |"""
  }

  case class OverridesNothingButNameExists(member: Symbol, existing: List[Denotations.SingleDenotation])(implicit ctx: Context)
  extends Message(OverridesNothingButNameExistsID) {
    val kind: String = "Reference"
    val msg: String = hl"""${member} has a different signature than the overridden declaration"""

    val existingDecl: String = existing.map(_.showDcl).mkString("  \n")

    val explanation: String =
      hl"""|There must be a non-final field or method with the name ${member.name} and the
           |same parameter list in a super class of ${member.owner} to override it.
           |
           |  ${member.showDcl}
           |
           |The super classes of ${member.owner} contain the following members
           |named ${member.name}:
           |  ${existingDecl}
           |"""
  }

  case class ForwardReferenceExtendsOverDefinition(value: Symbol, definition: Symbol)(implicit ctx: Context)
  extends Message(ForwardReferenceExtendsOverDefinitionID) {
    val kind: String = "Reference"
    val msg: String = hl"${definition.name} is a forward reference extending over the definition of ${value.name}"

    val explanation: String =
      hl"""|${definition.name} is used before you define it, and the definition of ${value.name}
           |appears between that use and the definition of ${definition.name}.
           |
           |Forward references are allowed only, if there are no value definitions between
           |the reference and the referred method definition.
           |
           |Define ${definition.name} before it is used,
           |or move the definition of ${value.name} so it does not appear between
           |the declaration of ${definition.name} and its use,
           |or define ${value.name} as lazy.
           |""".stripMargin
  }

  case class ExpectedTokenButFound(expected: Token, found: Token)(implicit ctx: Context)
  extends Message(ExpectedTokenButFoundID) {
    val kind: String = "Syntax"

    private val expectedText =
      if (Tokens.isIdentifier(expected)) "an identifier"
      else Tokens.showToken(expected)

    private val foundText = Tokens.showToken(found)

    val msg: String = hl"""${expectedText} expected, but ${foundText} found"""

    private val ifKeyword =
      if (Tokens.isIdentifier(expected) && Tokens.isKeyword(found))
        s"""
           |If you necessarily want to use $foundText as identifier, you may put it in backticks.""".stripMargin
      else
        ""
    val explanation: String = s"$ifKeyword"
  }

  case class MixedLeftAndRightAssociativeOps(op1: Name, op2: Name, op2LeftAssoc: Boolean)(implicit ctx: Context)
  extends Message(MixedLeftAndRightAssociativeOpsID) {
    val kind: String = "Syntax"
    val op1Asso: String = if (op2LeftAssoc) "which is right-associative" else "which is left-associative"
    val op2Asso: String = if (op2LeftAssoc) "which is left-associative" else "which is right-associative"
    val msg: String = hl"${op1} (${op1Asso}) and ${op2} ($op2Asso) have same precedence and may not be mixed"
    val explanation: String =
      s"""|The operators ${op1} and ${op2} are used as infix operators in the same expression,
          |but they bind to different sides:
          |${op1} is applied to the operand to its ${if (op2LeftAssoc) "right" else "left"}
          |${op2} is applied to the operand to its ${if (op2LeftAssoc) "left" else "right"}
          |As both have the same precedence the compiler can't decide which to apply first.
          |
          |You may use parenthesis to make the application order explicit,
          |or use method application syntax operand1.${op1}(operand2).
          |
          |Operators ending in a colon ${":"} are right-associative. All other operators are left-associative.
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
    val kind: String = "Usage"
    private val traitOrAbstract = if (isTrait) hl"a trait" else hl"abstract"
    val msg: String = hl"""${cls.name} is ${traitOrAbstract}; it cannot be instantiated"""
    val explanation: String =
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

  case class OverloadedOrRecursiveMethodNeedsResultType(cycleSym: Symbol)(implicit ctx: Context)
  extends Message(OverloadedOrRecursiveMethodNeedsResultTypeID) {
    val kind: String = "Cyclic"
    val msg: String = hl"""Overloaded or recursive $cycleSym needs return type"""
    val explanation: String =
      hl"""Case 1: $cycleSym is overloaded
          |If there are multiple methods named $cycleSym and at least one definition of
          |it calls another, you need to specify the calling method's return type.
          |
          |Case 2: $cycleSym is recursive
          |If $cycleSym calls itself on any path (even through mutual recursion), you need to specify the return type
          |of $cycleSym or of a definition it's mutually recursive with.
          |""".stripMargin
  }

  case class RecursiveValueNeedsResultType(cycleSym: Symbol)(implicit ctx: Context)
  extends Message(RecursiveValueNeedsResultTypeID) {
    val kind: String = "Cyclic"
    val msg: String = hl"""Recursive $cycleSym needs type"""
    val explanation: String =
      hl"""The definition of $cycleSym is recursive and you need to specify its type.
          |""".stripMargin
  }

  case class CyclicReferenceInvolving(denot: SymDenotation)(implicit ctx: Context)
  extends Message(CyclicReferenceInvolvingID) {
    val kind: String = "Cyclic"
    val msg: String = hl"""Cyclic reference involving $denot"""
    val explanation: String =
      hl"""|$denot is declared as part of a cycle which makes it impossible for the
           |compiler to decide upon ${denot.name}'s type.
           |To avoid this error, try giving ${denot.name} an explicit type.
           |""".stripMargin
  }

  case class CyclicReferenceInvolvingImplicit(cycleSym: Symbol)(implicit ctx: Context)
  extends Message(CyclicReferenceInvolvingImplicitID) {
    val kind: String = "Cyclic"
    val msg: String = hl"""Cyclic reference involving implicit $cycleSym"""
    val explanation: String =
      hl"""|$cycleSym is declared as part of a cycle which makes it impossible for the
           |compiler to decide upon ${cycleSym.name}'s type.
           |This might happen when the right hand-side of $cycleSym's definition involves an implicit search.
           |To avoid this error, try giving ${cycleSym.name} an explicit type.
           |""".stripMargin
  }

  case class SuperQualMustBeParent(qual: untpd.Ident, cls: ClassSymbol)(implicit ctx: Context)
  extends Message(SuperQualMustBeParentID) {

    val msg: String = hl"""|$qual does not name a parent of $cls"""
    val kind: String = "Reference"

    private val parents: Seq[String] = (cls.info.parents map (_.typeSymbol.name.show)).sorted

    val explanation: String =
      hl"""|When a qualifier ${"T"} is used in a ${"super"} prefix of the form ${"C.super[T]"},
           |${"T"} must be a parent type of ${"C"}.
           |
           |In this case, the parents of $cls are:
           |${parents.mkString("  - ", "\n  - ", "")}
           |""".stripMargin
  }

  case class VarArgsParamMustComeLast()(implicit ctx: Context)
  extends Message(IncorrectRepeatedParameterSyntaxID) {
    val msg: String = hl"""${"varargs"} parameter must come last"""
    val kind: String = "Syntax"
    val explanation: String =
      hl"""|The ${"varargs"} field must be the last field in the method signature.
           |Attempting to define a field in a method signature after a ${"varargs"} field is an error.
           |"""
  }

  case class AmbiguousImport(name: Name, newPrec: Int, prevPrec: Int, prevCtx: Context)(implicit ctx: Context)
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


    val msg: String =
      i"""|Reference to ${hl"$name"} is ambiguous
          |it is both ${bindingString(newPrec, ctx)}
          |and ${bindingString(prevPrec, prevCtx, " subsequently")}"""

    val kind: String = "Reference"

    val explanation: String =
      hl"""|The compiler can't decide which of the possible choices you
           |are referencing with $name.
           |Note:
           |- Definitions take precedence over imports
           |- Named imports take precedence over wildcard imports
           |- You may replace a name when imported using
           |  ${"import"} scala.{ $name => ${name.show + "Tick"} }
           |"""
  }

  case class MethodDoesNotTakeParameters(tree: tpd.Tree)(implicit ctx: Context)
  extends Message(MethodDoesNotTakeParametersId) {
    val kind: String = "Reference"

    def methodSymbol: Symbol = tpd.methPart(tree).symbol

    val msg: String = {
      val more = if (tree.isInstanceOf[tpd.Apply]) " more" else ""
      hl"${methodSymbol.showLocated} does not take$more parameters"
    }

    val explanation: String = {
      val isNullary = methodSymbol.info.isInstanceOf[ExprType]
      val addendum =
        if (isNullary) "\nNullary methods may not be called with parenthesis"
        else ""

      "You have specified more parameter lists as defined in the method definition(s)." + addendum
    }

  }

  case class AmbiguousOverload(tree: tpd.Tree, alts: List[SingleDenotation], pt: Type)(
    err: Errors)(
    implicit ctx: Context)
  extends Message(AmbiguousOverloadID) {

    private val all = if (alts.length == 2) "both" else "all"
    val msg: String =
      s"""|Ambiguous overload. The ${err.overloadedAltsStr(alts)}
          |$all match ${err.expectedTypeStr(pt)}""".stripMargin
    val kind: String = "Reference"
    val explanation: String =
      hl"""|There are ${alts.length} methods that could be referenced as the compiler knows too little
           |about the expected type.
           |You may specify the expected type e.g. by
           |- assigning it to a value with a specified type, or
           |- adding a type ascription as in ${"instance.myMethod: String => Int"}
           |"""
  }

  case class ReassignmentToVal(name: Name)(implicit ctx: Context)
    extends Message(ReassignmentToValID) {
    val kind: String = "Reference"
    val msg: String = hl"""Reassignment to val $name"""
    val explanation: String =
      hl"""|You can not assign a new value to $name as values can't be changed.
           |Keep in mind that every statement has a value, so you may e.g. use
           |  ${"val"} $name ${"= if (condition) 2 else 5"}
           |In case you need a reassignable name, you can declare it as
           |variable
           |  ${"var"} $name ${"="} ...
           |""".stripMargin
  }

  case class TypeDoesNotTakeParameters(tpe: Type, params: List[Trees.Tree[Trees.Untyped]])(implicit ctx: Context)
    extends Message(TypeDoesNotTakeParametersID) {
    val kind: String = "Reference"
    val msg: String = hl"$tpe does not take type parameters"

    private val ps =
      if (params.size == 1) s"a type parameter ${params.head}"
      else s"type parameters ${params.map(_.show).mkString(", ")}"

    val explanation: String =
      i"""You specified ${NoColor(ps)} for ${hl"$tpe"}, which is not
         |declared to take any.
         |"""
  }

  case class ParameterizedTypeLacksArguments(psym: Symbol)(implicit ctx: Context)
    extends Message(ParameterizedTypeLacksArgumentsID) {
    val msg: String = hl"Parameterized $psym lacks argument list"
    val kind: String = "Reference"
    val explanation: String =
      hl"""The $psym is declared with non-implicit parameters, you may not leave
          |out the parameter list when extending it.
          |"""
  }

  case class VarValParametersMayNotBeCallByName(name: TermName, mutable: Boolean)(implicit ctx: Context)
    extends Message(VarValParametersMayNotBeCallByNameID) {
    val varOrVal = if (mutable) hl"${"var"}" else hl"${"val"}"
    val msg: String = s"$varOrVal parameters may not be call-by-name"
    val kind: String = "Syntax"
    val explanation: String =
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
    val msg: String =
      if (tpe.derivesFrom(defn.AnyKindClass)) hl"${tpe} cannot be used as a value type"
      else hl"Missing type parameter for ${tpe}"
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class DoesNotConformToBound(tpe: Type, which: String, bound: Type)(
    err: Errors)(implicit ctx: Context)
    extends Message(DoesNotConformToBoundID) {
    val msg: String = hl"Type argument ${tpe} does not conform to $which bound $bound ${err.whyNoMatchStr(tpe, bound)}"
    val kind: String = "Type Mismatch"
    val explanation: String = ""
  }

  case class DoesNotConformToSelfType(category: String, selfType: Type, cls: Symbol,
                                      otherSelf: Type, relation: String, other: Symbol)(
    implicit ctx: Context)
    extends Message(DoesNotConformToSelfTypeID) {
    val msg: String = hl"""$category: self type $selfType of $cls does not conform to self type $otherSelf
                  |of $relation $other"""
    val kind: String = "Type Mismatch"
    val explanation: String =
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
    val msg: String = hl"""$tp does not conform to its self type $selfType; cannot be instantiated"""
    val kind: String = "Type Mismatch"
    val explanation: String =
      hl"""To create an instance of $tp it needs to inherit $selfType in some way.
          |
          |Note: Self types are indicated with the notation
          |  ${s"class "}$tp ${"{ this: "}$selfType${" => "}
          |"""
  }

  case class AbstractMemberMayNotHaveModifier(sym: Symbol, flag: FlagSet)(
    implicit ctx: Context)
    extends Message(AbstractMemberMayNotHaveModifierID) {
    val msg: String = hl"""${"abstract"} $sym may not have `$flag' modifier"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class TopLevelCantBeImplicit(sym: Symbol)(
    implicit ctx: Context)
    extends Message(TopLevelCantBeImplicitID) {
    val msg: String = hl"""${"implicit"} modifier cannot be used for top-level definitions"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class TypesAndTraitsCantBeImplicit()(implicit ctx: Context)
    extends Message(TypesAndTraitsCantBeImplicitID) {
    val msg: String = hl"""${"implicit"} modifier cannot be used for types or traits"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class OnlyClassesCanBeAbstract(sym: Symbol)(
    implicit ctx: Context)
    extends Message(OnlyClassesCanBeAbstractID) {
    val msg: String = hl"""${"abstract"} modifier can be used only for classes; it should be omitted for abstract members"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class AbstractOverrideOnlyInTraits(sym: Symbol)(
    implicit ctx: Context)
    extends Message(AbstractOverrideOnlyInTraitsID) {
    val msg: String = hl"""${"abstract override"} modifier only allowed for members of traits"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class TraitsMayNotBeFinal(sym: Symbol)(
    implicit ctx: Context)
    extends Message(TraitsMayNotBeFinalID) {
    val msg: String = hl"""$sym may not be ${"final"}"""
    val kind: String = "Syntax"
    val explanation: String =
      "A trait can never be final since it is abstract and must be extended to be useful."
  }

  case class NativeMembersMayNotHaveImplementation(sym: Symbol)(
    implicit ctx: Context)
    extends Message(NativeMembersMayNotHaveImplementationID) {
    val msg: String = hl"""${"@native"} members may not have an implementation"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class OnlyClassesCanHaveDeclaredButUndefinedMembers(sym: Symbol)(
    implicit ctx: Context)
    extends Message(OnlyClassesCanHaveDeclaredButUndefinedMembersID) {

    private val varNote =
      if (sym.is(Mutable)) "Note that variables need to be initialized to be defined."
      else ""
    val msg: String = hl"""Declaration of $sym not allowed here: only classes can have declared but undefined members"""
    val kind: String = "Syntax"
    val explanation: String = s"$varNote"
  }

  case class CannotExtendAnyVal(sym: Symbol)(implicit ctx: Context)
    extends Message(CannotExtendAnyValID) {
    val msg: String = hl"""$sym cannot extend ${"AnyVal"}"""
    val kind: String = "Syntax"
    val explanation: String =
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

    val msg: String = hl"""$sym cannot have the same name as ${cls.showLocated} -- """ + reasonMessage
    val kind: String = "Syntax"
    val explanation: String = ""
  }
  object CannotHaveSameNameAs {
    sealed trait Reason
    case object CannotBeOverridden extends Reason
    case class DefinedInSelf(self: tpd.ValDef) extends Reason
  }

  case class ValueClassesMayNotDefineInner(valueClass: Symbol, inner: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotDefineInnerID) {
    val msg: String = hl"""Value classes may not define an inner class"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ValueClassesMayNotDefineNonParameterField(valueClass: Symbol, field: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotDefineNonParameterFieldID) {
    val msg: String = hl"""Value classes may not define non-parameter field"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ValueClassesMayNotDefineASecondaryConstructor(valueClass: Symbol, constructor: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotDefineASecondaryConstructorID) {
    val msg: String = hl"""Value classes may not define a secondary constructor"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ValueClassesMayNotContainInitalization(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotContainInitalizationID) {
    val msg: String = hl"""Value classes may not contain initialization statements"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ValueClassesMayNotBeAbstract(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotBeAbstractID) {
    val msg: String = hl"""Value classes may not be ${"abstract"}"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ValueClassesMayNotBeContainted(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotBeContaintedID) {
    private val localOrMember = if (valueClass.owner.isTerm) "local class" else "member of another class"
    val msg: String = s"""Value classes may not be a $localOrMember"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ValueClassesMayNotWrapItself(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassesMayNotWrapItselfID) {
    val msg: String = """A value class may not wrap itself"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ValueClassParameterMayNotBeAVar(valueClass: Symbol, param: Symbol)(implicit ctx: Context)
    extends Message(ValueClassParameterMayNotBeAVarID) {
    val msg: String = hl"""A value class parameter may not be a ${"var"}"""
    val kind: String = "Syntax"
    val explanation: String =
      hl"""A value class must have exactly one ${"val"} parameter."""
  }

  case class ValueClassNeedsOneValParam(valueClass: Symbol)(implicit ctx: Context)
    extends Message(ValueClassNeedsExactlyOneValParamID) {
    val msg: String = hl"""Value class needs one ${"val"} parameter"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ValueClassParameterMayNotBeCallByName(valueClass: Symbol, param: Symbol)(implicit ctx: Context)
    extends Message(ValueClassParameterMayNotBeCallByNameID) {
    val msg: String = s"Value class parameter `${param.name}` may not be call-by-name"
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class OnlyCaseClassOrCaseObjectAllowed()(implicit ctx: Context)
    extends Message(OnlyCaseClassOrCaseObjectAllowedID) {
    val msg: String = hl"""Only ${"case class"} or ${"case object"} allowed"""
    val kind: String = "Syntax"
    val explanation: String = ""
  }

  case class ExpectedToplevelDef()(implicit ctx: Context)
    extends Message(ExpectedTopLevelDefID) {
    val kind: String = "Syntax"
    val msg: String = "Expected a toplevel definition"
    val explanation: String = ""
  }

  case class SuperCallsNotAllowedInlineable(symbol: Symbol)(implicit ctx: Context)
    extends Message(SuperCallsNotAllowedInlineableID) {
    val kind: String = "Syntax"
    val msg: String = s"Super call not allowed in inlineable $symbol"
    val explanation: String = "Method inlining prohibits calling superclass methods, as it may lead to confusion about which super is being called."
  }

  case class ModifiersNotAllowed(flags: FlagSet, printableType: Option[String])(implicit ctx: Context)
    extends Message(ModifiersNotAllowedID) {
    val kind: String = "Syntax"
    val msg: String = hl"Modifier(s) $flags not allowed for ${printableType.getOrElse("combination")}"
    val explanation: String = {
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

  case class WrongNumberOfParameters(expected: Int)(implicit ctx: Context)
    extends Message(WrongNumberOfParametersID) {
    val kind: String = "Syntax"
    val msg: String = s"Wrong number of parameters, expected: $expected"
    val explanation: String = ""
  }

  case class DuplicatePrivateProtectedQualifier()(implicit ctx: Context)
    extends Message(DuplicatePrivateProtectedQualifierID) {
    val kind: String = "Syntax"
    val msg: String = "Duplicate private/protected qualifier"
    val explanation: String =
      hl"It is not allowed to combine `private` and `protected` modifiers even if they are qualified to different scopes"
  }

  case class ExpectedStartOfTopLevelDefinition()(implicit ctx: Context)
    extends Message(ExpectedStartOfTopLevelDefinitionID) {
    val kind: String = "Syntax"
    val msg: String = "Expected start of definition"
    val explanation: String =
      hl"You have to provide either ${"class"}, ${"trait"}, ${"object"}, or ${"enum"} definitions after qualifiers"
  }

  case class NoReturnFromInlineable(owner: Symbol)(implicit ctx: Context)
    extends Message(NoReturnFromInlineableID) {
    val kind: String = "Syntax"
    val msg: String = hl"No explicit ${"return"} allowed from inlineable $owner"
    val explanation: String =
      hl"""Methods marked with ${"inline"} modifier may not use ${"return"} statements.
          |Instead, you should rely on the last expression's value being
          |returned from a method.
          |"""
  }

  case class ReturnOutsideMethodDefinition(owner: Symbol)(implicit ctx: Context)
    extends Message(ReturnOutsideMethodDefinitionID) {
    val kind: String = "Syntax"
    val msg: String = hl"${"return"} outside method definition"
    val explanation: String =
      hl"""You used ${"return"} in ${owner}.
          |${"return"} is a keyword and may only be used within method declarations.
          |"""
  }

  case class ExtendFinalClass(clazz:Symbol, finalClazz: Symbol)(implicit ctx: Context)
    extends Message(ExtendFinalClassID) {
    val kind: String = "Syntax"
    val msg: String = hl"$clazz cannot extend ${"final"} $finalClazz"
    val explanation: String =
      hl"""A class marked with the ${"final"} keyword cannot be extended"""
  }

  case class ExpectedTypeBoundOrEquals(found: Token)(implicit ctx: Context)
    extends Message(ExpectedTypeBoundOrEqualsID) {
    val kind: String = "Syntax"
    val msg: String = hl"${"="}, ${">:"}, or ${"<:"} expected, but ${Tokens.showToken(found)} found"

    val explanation: String =
      hl"""Type parameters and abstract types may be constrained by a type bound.
           |Such type bounds limit the concrete values of the type variables and possibly
           |reveal more information about the members of such types.
           |
           |A lower type bound ${"B >: A"} expresses that the type variable ${"B"}
           |refers to a supertype of type ${"A"}.
           |
           |An upper type bound ${"T <: A"} declares that type variable ${"T"}
           |refers to a subtype of type ${"A"}.
           |"""
  }

  case class ClassAndCompanionNameClash(cls: Symbol, other: Symbol)(implicit ctx: Context)
    extends Message(ClassAndCompanionNameClashID) {
    val kind: String = "Naming"
    val msg: String = hl"Name clash: both ${cls.owner} and its companion object defines ${cls.name.stripModuleClassSuffix}"
    val explanation: String = {
      val kind = if (cls.owner.is(Flags.Trait)) "trait" else "class"

      hl"""|A $kind and its companion object cannot both define a ${"class"}, ${"trait"} or ${"object"} with the same name:
           |  - ${cls.owner} defines ${cls}
           |  - ${other.owner} defines ${other}"""
      }
  }

  case class TailrecNotApplicable(symbol: Symbol)(implicit ctx: Context)
    extends Message(TailrecNotApplicableID) {
    val kind: String = "Syntax"
    val msg: String = {
      val reason =
        if (!symbol.is(Method)) hl"$symbol isn't a method"
        else if (symbol.is(Deferred)) hl"$symbol is abstract"
        else if (!symbol.isEffectivelyFinal) hl"$symbol is neither ${"private"} nor ${"final"} so can be overridden"
        else hl"$symbol contains no recursive calls"

      s"TailRec optimisation not applicable, $reason"
    }
    val explanation: String = ""
  }

  case class FailureToEliminateExistential(tp: Type, tp1: Type, tp2: Type, boundSyms: List[Symbol])(implicit ctx: Context)
    extends Message(FailureToEliminateExistentialID) {
    val kind: String = "Compatibility"
    val msg: String = "Failure to eliminate existential type. Proceed at own risk."
    val explanation: String = {
      val originalType = ctx.printer.dclsText(boundSyms, "; ").show
      hl"""original type    : $tp forSome ${originalType}
          |reduces to       : $tp1
          |type used instead: $tp2"""
    }
  }

  case class OnlyFunctionsCanBeFollowedByUnderscore(tp: Type)(implicit ctx: Context)
    extends Message(OnlyFunctionsCanBeFollowedByUnderscoreID) {
    val kind: String = "Syntax"
    val msg: String = hl"Only function types can be followed by ${"_"} but the current expression has type $tp"
    val explanation: String =
      hl"""The syntax ${"x _"} is no longer supported if ${"x"} is not a function.
          |To convert to a function value, you need to explicitly write ${"() => x"}"""
  }

  case class MissingEmptyArgumentList(method: Symbol)(implicit ctx: Context)
    extends Message(MissingEmptyArgumentListID) {
    val kind: String = "Syntax"
    val msg: String = hl"$method must be called with ${"()"} argument"
    val explanation: String = {
      val codeExample =
        """def next(): T = ...
          |next     // is expanded to next()"""

      hl"""Previously an empty argument list () was implicitly inserted when calling a nullary method without arguments. E.g.
          |
          |$codeExample
          |
          |In Dotty, this idiom is an error. The application syntax has to follow exactly the parameter syntax.
          |Excluded from this rule are methods that are defined in Java or that override methods defined in Java."""
    }
  }

  case class DuplicateNamedTypeParameter(name: Name)(implicit ctx: Context)
    extends Message(DuplicateNamedTypeParameterID) {
    val kind: String = "Syntax"
    val msg: String = hl"Type parameter $name was defined multiple times."
    val explanation: String = ""
  }

  case class UndefinedNamedTypeParameter(undefinedName: Name, definedNames: List[Name])(implicit ctx: Context)
    extends Message(UndefinedNamedTypeParameterID) {
    val kind: String = "Syntax"
    val msg: String = hl"Type parameter $undefinedName is undefined. Expected one of ${definedNames.map(_.show).mkString(", ")}."
    val explanation: String = ""
  }

  case class IllegalStartOfStatement(isModifier: Boolean)(implicit ctx: Context) extends Message(IllegalStartOfStatementID) {
    val kind: String = "Syntax"
    val msg: String = {
      val addendum = if (isModifier) ": no modifiers allowed here" else ""
      "Illegal start of statement" + addendum
    }
    val explanation: String = "A statement is either an import, a definition or an expression."
  }

  case class TraitIsExpected(symbol: Symbol)(implicit ctx: Context) extends Message(TraitIsExpectedID) {
    val kind: String = "Syntax"
    val msg: String = hl"$symbol is not a trait"
    val explanation: String = {
      val errorCodeExample =
        """class A
          |class B
          |
          |val a = new A with B // will fail with a compile error - class B is not a trait""".stripMargin
      val codeExample =
        """class A
          |trait B
          |
          |val a = new A with B // compiles normally""".stripMargin

      hl"""Only traits can be mixed into classes using a ${"with"} keyword.
          |Consider the following example:
          |
          |$errorCodeExample
          |
          |The example mentioned above would fail because B is not a trait.
          |But if you make B a trait it will be compiled without any errors:
          |
          |$codeExample
          |"""
    }
  }

  case class TraitRedefinedFinalMethodFromAnyRef(method: Symbol)(implicit ctx: Context) extends Message(TraitRedefinedFinalMethodFromAnyRefID) {
    val kind: String = "Syntax"
    val msg: String = hl"Traits cannot redefine final $method from ${"class AnyRef"}."
    val explanation: String = ""
  }

  case class PackageNameAlreadyDefined(pkg: Symbol)(implicit ctx: Context) extends Message(PackageNameAlreadyDefinedID) {
    val msg: String = hl"${pkg} is already defined, cannot be a ${"package"}"
    val kind: String = "Naming"
    val explanation: String =
      hl"An ${"object"} cannot have the same name as an existing ${"package"}. Rename either one of them."
  }

  case class UnapplyInvalidNumberOfArguments(qual: untpd.Tree, argTypes: List[Type])(implicit ctx: Context)
    extends Message(UnapplyInvalidNumberOfArgumentsID) {
    val kind: String = "Syntax"
    val msg: String = hl"Wrong number of argument patterns for $qual; expected: ($argTypes%, %)"
    val explanation: String =
      hl"""The Unapply method of $qual was used with incorrect number of arguments.
          |Expected usage would be something like:
          |case $qual(${argTypes.map(_ => '_')}%, %) => ...
          |
        |where subsequent arguments would have following types: ($argTypes%, %).
        |""".stripMargin
  }

  case class UnapplyInvalidReturnType(unapplyResult: Type, unapplyName: Symbol#ThisName)(implicit ctx: Context)
    extends Message(UnapplyInvalidReturnTypeID) {
    val kind = "Type Mismatch"
    val addendum =
      if (ctx.scala2Mode && unapplyName == nme.unapplySeq)
        "\nYou might want to try to rewrite the extractor to use `unapply` instead."
      else ""
    val msg = hl"""| ${Red(i"$unapplyResult")} is not a valid result type of an $unapplyName method of an ${Magenta("extractor")}.$addendum"""
    val explanation = if (unapplyName.show == "unapply")
      hl"""
          |To be used as an extractor, an unapply method has to return a type that either:
          | - has members ${Magenta("isEmpty: Boolean")} and ${Magenta("get: S")} (usually an ${Green("Option[S]")})
          | - is a ${Green("Boolean")}
          | - is a ${Green("Product")} (like a ${Magenta("Tuple2[T1, T2]")})
          |
          |class A(val i: Int)
          |
          |object B {
          |  def unapply(a: A): ${Green("Option[Int]")} = Some(a.i)
          |}
          |
          |object C {
          |  def unapply(a: A): ${Green("Boolean")} = a.i == 2
          |}
          |
          |object D {
          |  def unapply(a: A): ${Green("(Int, Int)")} = (a.i, a.i)
          |}
          |
          |object Test {
          |  def test(a: A) = a match {
          |    ${Magenta("case B(1)")} => 1
          |    ${Magenta("case a @ C()")} => 2
          |    ${Magenta("case D(3, 3)")} => 3
          |  }
          |}
        """.stripMargin
    else
      hl"""
          |To be used as an extractor, an unapplySeq method has to return a type which has members
          |${Magenta("isEmpty: Boolean")} and ${Magenta("get: S")} where ${Magenta("S <: Seq[V]")} (usually an ${Green("Option[Seq[V]]")}):
          |
          |object CharList {
          |  def unapplySeq(s: String): ${Green("Option[Seq[Char]")} = Some(s.toList)
          |
          |  "example" match {
          |    ${Magenta("case CharList(c1, c2, c3, c4, _, _, _)")} =>
          |      println(s"$$c1,$$c2,$$c3,$$c4")
          |    case _ =>
          |      println("Expected *exactly* 7 characters!")
          |  }
          |}
        """.stripMargin
  }

  case class StaticFieldsOnlyAllowedInObjects(member: Symbol)(implicit ctx: Context) extends Message(StaticFieldsOnlyAllowedInObjectsID) {
    val msg: String = hl"${"@static"} $member in ${member.owner} must be defined inside an ${"object"}."
    val kind: String = "Syntax"
    val explanation: String =
      hl"${"@static"} members are only allowed inside objects."
  }

  case class StaticFieldsShouldPrecedeNonStatic(member: Symbol, defns: List[tpd.Tree])(implicit ctx: Context) extends Message(StaticFieldsShouldPrecedeNonStaticID) {
    val msg: String = hl"${"@static"} $member in ${member.owner} must be defined before non-static fields."
    val kind: String = "Syntax"

    val explanation: String = {
      val nonStatics = defns.takeWhile(_.symbol != member).take(3).filter(_.isInstanceOf[tpd.ValDef])
      val codeExample = s"""object ${member.owner.name.firstPart} {
                        |  @static ${member} = ...
                        |  ${nonStatics.map(m => s"${m.symbol} = ...").mkString("\n  ")}
                        |  ...
                        |}"""
      hl"""The fields annotated with @static should precede any non @static fields.
        |This ensures that we do not introduce surprises for users in initialization order of this class.
        |Static field are initialized when class loading the code of Foo.
        |Non static fields are only initialized the first  time that Foo is accessed.
        |
        |The definition of ${member.name} should have been before the non ${"@static val"}s:
        |$codeExample
        |"""
    }
  }

  case class CyclicInheritance(symbol: Symbol, addendum: String)(implicit ctx: Context) extends Message(CyclicInheritanceID) {
    val kind: String = "Syntax"
    val msg: String = hl"Cyclic inheritance: $symbol extends itself$addendum"
    val explanation: String = {
      val codeExample = "class A extends A"

      hl"""Cyclic inheritance is prohibited in Dotty.
          |Consider the following example:
          |
          |$codeExample
          |
          |The example mentioned above would fail because this type of inheritance hierarchy
          |creates a "cycle" where a not yet defined class A extends itself which makes
          |impossible to instantiate an object of this class"""
    }
  }

  case class BadSymbolicReference(denot: SymDenotation)(implicit ctx: Context) extends Message(BadSymbolicReferenceID) {
    val kind: String = "Reference"

    val msg: String = {
      val denotationOwner = denot.owner
      val denotationName = ctx.fresh.setSetting(ctx.settings.YdebugNames, true).printer.nameString(denot.name)
      val file = denot.symbol.associatedFile
      val (location, src) =
        if (file != null) (s" in $file", file.toString)
        else ("", "the signature")

      hl"""Bad symbolic reference. A signature$location
          |refers to $denotationName in ${denotationOwner.showKind} ${denotationOwner.showFullName} which is not available.
          |It may be completely missing from the current classpath, or the version on
          |the classpath might be incompatible with the version used when compiling $src."""
    }

    val explanation: String = ""
  }

  case class UnableToExtendSealedClass(pclazz: Symbol)(implicit ctx: Context) extends Message(UnableToExtendSealedClassID) {
    val kind: String = "Syntax"
    val msg: String = hl"Cannot extend ${"sealed"} $pclazz in a different source file"
    val explanation: String = "A sealed class or trait can only be extended in the same file as its declaration"
  }

  case class SymbolHasUnparsableVersionNumber(symbol: Symbol, migrationMessage: String)(implicit ctx: Context)
  extends Message(SymbolHasUnparsableVersionNumberID) {
    val kind: String = "Syntax"
    val msg: String = hl"${symbol.showLocated} has an unparsable version number: $migrationMessage"
    val explanation: String =
      hl"""$migrationMessage
          |
          |The ${symbol.showLocated} is marked with ${"@migration"} indicating it has changed semantics
          |between versions and the ${"-Xmigration"} settings is used to warn about constructs
          |whose behavior may have changed since version change."""
  }

  case class SymbolChangedSemanticsInVersion(
    symbol: Symbol,
    migrationVersion: ScalaVersion
  )(implicit ctx: Context) extends Message(SymbolChangedSemanticsInVersionID) {
    val kind: String = "Syntax"
    val msg: String = hl"${symbol.showLocated} has changed semantics in version $migrationVersion"
    val explanation: String = {
      hl"""The ${symbol.showLocated} is marked with ${"@migration"} indicating it has changed semantics
          |between versions and the ${"-Xmigration"} settings is used to warn about constructs
          |whose behavior may have changed since version change."""
    }
  }

  case class UnableToEmitSwitch(tooFewCases: Boolean)(implicit ctx: Context)
  extends Message(UnableToEmitSwitchID) {
    val kind: String = "Syntax"
    val tooFewStr: String = if (tooFewCases) " since there are not enough cases" else ""
    val msg: String = hl"Could not emit switch for ${"@switch"} annotated match$tooFewStr"
    val explanation: String = {
      val codeExample =
        """val ConstantB = 'B'
          |final val ConstantC = 'C'
          |def tokenMe(ch: Char) = (ch: @switch) match {
          |  case '\t' | '\n' => 1
          |  case 'A'         => 2
          |  case ConstantB   => 3  // a non-literal may prevent switch generation: this would not compile
          |  case ConstantC   => 4  // a constant value is allowed
          |  case _           => 5
          |}""".stripMargin

      hl"""If annotated with ${"@switch"}, the compiler will verify that the match has been compiled to a
          |tableswitch or lookupswitch and issue an error if it instead compiles into a series of conditional
          |expressions. Example usage:
          |
          |$codeExample
          |
          |The compiler will not apply the optimisation if:
          |- the matched value is not of type ${"Int"}, ${"Byte"}, ${"Short"} or ${"Char"}
          |- the matched value is not a constant literal
          |- there are less than three cases"""
    }
  }

  case class MissingCompanionForStatic(member: Symbol)(implicit ctx: Context) extends Message(MissingCompanionForStaticID) {
    val msg: String = hl"${member.owner} does not have a companion class"
    val kind: String = "Syntax"
    val explanation: String =
      hl"An object that contains ${"@static"} members must have a companion class."
  }

  case class PolymorphicMethodMissingTypeInParent(rsym: Symbol, parentSym: Symbol)(implicit ctx: Context)
  extends Message(PolymorphicMethodMissingTypeInParentID) {
    val kind: String = "Syntax"
    val msg: String = hl"Polymorphic refinement $rsym without matching type in parent $parentSym is no longer allowed"
    val explanation: String =
      hl"""Polymorphic $rsym is not allowed in the structural refinement of $parentSym because
          |$rsym does not override any method in $parentSym. Structural refinement does not allow for
          |polymorphic methods."""
  }

  case class ParamsNoInline(owner: Symbol)(implicit ctx: Context)
    extends Message(ParamsNoInlineID) {
    val kind: String = "Syntax"
    val msg: String = hl"""${"inline"} modifier can only be used for parameters of inline methods"""
    val explanation: String = ""
  }

  case class JavaSymbolIsNotAValue(symbol: Symbol)(implicit ctx: Context) extends Message(JavaSymbolIsNotAValueID) {
    val kind: String = "Type Mismatch"
    val msg: String = {
      val kind =
        if (symbol is Package) hl"$symbol"
        else hl"Java defined ${"class " + symbol.name}"

      s"$kind is not a value"
    }
    val explanation: String = ""
  }

  case class DoubleDefinition(decl: Symbol, previousDecl: Symbol, base: Symbol)(implicit ctx: Context) extends Message(DoubleDefinitionID) {
    val kind: String = "Duplicate Symbol"
    val msg: String = {
      val details = if (decl.isRealMethod && previousDecl.isRealMethod) {
        // compare the signatures when both symbols represent methods
        decl.signature.matchDegree(previousDecl.signature) match {
          case Signature.NoMatch =>
            "" // shouldn't be reachable
          case Signature.ParamMatch =>
            "have matching parameter types."
          case Signature.FullMatch =>
            "have the same type after erasure."
        }
      } else ""
      def symLocation(sym: Symbol) = {
        val lineDesc =
          if (sym.span.exists && sym.span != sym.owner.span)
            s" at line ${sym.sourcePos.line + 1}" else ""
        i"in ${sym.owner}${lineDesc}"
      }
      val clashDescription =
        if (decl.owner eq previousDecl.owner)
          "Double definition"
        else if ((decl.owner eq base) || (previousDecl eq base))
          "Name clash between defined and inherited member"
        else
          "Name clash between inherited members"

      em"""$clashDescription:
          |${previousDecl.showDcl} ${symLocation(previousDecl)} and
          |${decl.showDcl} ${symLocation(decl)}
          |""" + details
    }
    val explanation: String = ""
  }

  case class ImportRenamedTwice(ident: untpd.Ident)(implicit ctx: Context) extends Message(ImportRenamedTwiceID) {
    val kind: String = "Syntax"
    val msg: String = s"${ident.show} is renamed twice on the same import line."
    val explanation: String = ""
  }

  case class TypeTestAlwaysSucceeds(foundCls: Symbol, testCls: Symbol)(implicit ctx: Context) extends Message(TypeTestAlwaysSucceedsID) {
    val kind: String = "Syntax"
    val msg: String = {
      val addendum =
        if (foundCls != testCls) s" is a subtype of $testCls"
        else " is the same as the tested type"
      s"The highlighted type test will always succeed since the scrutinee type ($foundCls)" + addendum
    }
    val explanation: String = ""
  }

  // Relative of CyclicReferenceInvolvingImplicit and RecursiveValueNeedsResultType
  case class TermMemberNeedsResultTypeForImplicitSearch(cycleSym: Symbol)(implicit ctx: Context)
    extends Message(TermMemberNeedsNeedsResultTypeForImplicitSearchID) {
    val kind: String = "Cyclic"
    val msg: String = hl"""$cycleSym needs result type because its right-hand side attempts implicit search"""
    val explanation: String =
      hl"""|The right hand-side of $cycleSym's definition requires an implicit search at the highlighted position.
           |To avoid this error, give `$cycleSym` an explicit type.
           |""".stripMargin
  }

  case class ClassCannotExtendEnum(cls: Symbol, parent: Symbol)(implicit ctx: Context) extends Message(ClassCannotExtendEnumID) {
    override def kind: String = "Syntax"
    override def msg: String = hl"""$cls in ${cls.owner} extends enum ${parent.name}, but extending enums is prohibited."""
    override def explanation: String = ""
  }

  case class NotAnExtractor(tree: untpd.Tree)(implicit ctx: Context) extends Message(NotAnExtractorID) {
    override def msg: String = hl"$tree cannot be used as an extractor in a pattern because it lacks an unapply or unapplySeq method"
    override def kind: String = "Syntax"
    override def explanation: String =
      hl"""|An ${"unapply"} method should be defined in an ${"object"} as follow:
           |  - If it is just a test, return a ${"Boolean"}. For example ${"case even()"}
           |  - If it returns a single sub-value of type T, return an ${"Option[T]"}
           |  - If it returns several sub-values T1,...,Tn, group them in an optional tuple ${"Option[(T1,...,Tn)]"}
           |
           |Sometimes, the number of sub-values isn't fixed and we would like to return a sequence.
           |For this reason, you can also define patterns through ${"unapplySeq"} which returns ${"Option[Seq[T]]"}.
           |This mechanism is used for instance in pattern ${"case List(x1, ..., xn)"}""".stripMargin
  }

  case class MemberWithSameNameAsStatic()(implicit val ctx: Context)
    extends Message(MemberWithSameNameAsStaticID) {

    override def msg: String = hl"Companion classes cannot define members with same name as a ${"@static"} member"
    override def kind: String = "Syntax"
    override def explanation: String = ""
  }

  case class PureExpressionInStatementPosition(stat: untpd.Tree, exprOwner: Symbol)(implicit ctx: Context)
    extends Message(PureExpressionInStatementPositionID) {

    val kind = "Potential Issue"
    val msg = "A pure expression does nothing in statement position; you may be omitting necessary parentheses"
    val explanation =
      hl"""The pure expression $stat doesn't have any side effect and its result is not assigned elsewhere.
          |It can be removed without changing the semantics of the program. This may indicate an error.""".stripMargin
  }

  case class TraitCompanionWithMutableStatic()(implicit val ctx: Context)
    extends Message(TraitCompanionWithMutableStaticID) {
    override def msg: String = hl"Companion of traits cannot define mutable @static fields"
    override def kind: String = "Syntax"
    override def explanation: String = ""
  }

  case class LazyStaticField()(implicit val ctx: Context)
    extends Message(LazyStaticFieldID) {
    override def msg: String = hl"Lazy @static fields are not supported"
    override def kind: String = "Syntax"
    override def explanation: String = ""
  }

  case class StaticOverridingNonStaticMembers()(implicit val ctx: Context)
    extends Message(StaticOverridingNonStaticMembersID) {
    override def msg: String = hl"${"@static"} members cannot override or implement non-static ones"
    override def kind: String = "Syntax"
    override def explanation: String = ""
  }

  case class OverloadInRefinement(rsym: Symbol)(implicit val ctx: Context)
    extends Message(OverloadInRefinementID) {
      override def msg: String = "Refinements cannot introduce overloaded definitions"
      override def kind: String = "Overload"
      override def explanation: String =
        hl"""The refinement `$rsym` introduces an overloaded definition.
            |Refinements cannot contain overloaded definitions.""".stripMargin
    }

  case class NoMatchingOverload(alternatives: List[SingleDenotation], pt: Type)(
    err: Errors)(implicit val ctx: Context)
    extends Message(NoMatchingOverloadID) {
    val msg: String =
      hl"""None of the ${err.overloadedAltsStr(alternatives)}
          |match ${err.expectedTypeStr(pt)}"""
    val kind: String = "Type Mismatch"
    val explanation: String = ""
  }
  case class StableIdentPattern(tree: untpd.Tree, pt: Type)(implicit val ctx: Context)
    extends Message(StableIdentPatternID) {
    override def kind: String = "Syntax"
    override def msg: String =
      hl"""Stable identifier required, but ${tree.show} found"""
    override def explanation: String = ""
  }

  case class IllegalSuperAccessor(base: Symbol, memberName: Name,
      acc: Symbol, accTp: Type,
      other: Symbol, otherTp: Type)(implicit val ctx: Context) extends Message(IllegalSuperAccessorID) {
    val kind: String = "Reference"
    val msg: String = {
      // The mixin containing a super-call that requires a super-accessor
      val accMixin = acc.owner
      // The class or trait that the super-accessor should resolve too in `base`
      val otherMixin = other.owner
      // The super-call in `accMixin`
      val superCall = i"super.$memberName"
      // The super-call that the super-accesors in `base` forwards to
      val resolvedSuperCall = i"super[${otherMixin.name}].$memberName"
      // The super-call that we would have called if `super` in traits behaved like it
      // does in classes, i.e. followed the linearization of the trait itself.
      val staticSuperCall = {
        val staticSuper = accMixin.asClass.info.parents.reverse
          .find(_.nonPrivateMember(memberName).matchingDenotation(accMixin.thisType, acc.info).exists)
        val staticSuperName = staticSuper match {
          case Some(parent) =>
            parent.classSymbol.name.show
          case None => // Might be reachable under separate compilation
            "SomeParent"
        }
        i"super[$staticSuperName].$memberName"
      }
      hl"""$base cannot be defined due to a conflict between its parents when
          |implementing a super-accessor for $memberName in $accMixin:
          |
          |1. One of its parent (${accMixin.name}) contains a call $superCall in its body,
          |   and when a super-call in a trait is written without an explicit parent
          |   listed in brackets, it is implemented by a generated super-accessor in
          |   the class that extends this trait based on the linearization order of
          |   the class.
          |2. Because ${otherMixin.name} comes before ${accMixin.name} in the linearization
          |   order of ${base.name}, and because ${otherMixin.name} overrides $memberName,
          |   the super-accessor in ${base.name} is implemented as a call to
          |   $resolvedSuperCall.
          |3. However,
          |   ${otherTp.widenExpr} (the type of $resolvedSuperCall in ${base.name})
          |   is not a subtype of
          |   ${accTp.widenExpr} (the type of $memberName in $accMixin).
          |   Hence, the super-accessor that needs to be generated in ${base.name}
          |   is illegal.
          |
          |Here are two possible ways to resolve this:
          |
          |1. Change the linearization order of ${base.name} such that
          |   ${accMixin.name} comes before ${otherMixin.name}.
          |2. Alternatively, replace $superCall in the body of $accMixin by a
          |   super-call to a specific parent, e.g. $staticSuperCall
          |""".stripMargin
    }
    val explanation: String = ""
  }
}
