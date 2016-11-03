package dotty.tools
package dotc
package reporting
package diagnostic

import dotc.core._
import Contexts.Context, Decorators._, Symbols._, Names._, NameOps._, Types._
import util.{SourceFile, NoSource}
import util.{SourcePosition, NoSourcePosition}
import config.Settings.Setting
import interfaces.Diagnostic.{ERROR, WARNING, INFO}
import printing.Highlighting._
import printing.Formatting

object messages {

  // `MessageContainer`s to be consumed by `Reporter` ---------------------- //
  class Error(
    msgFn: => Message,
    pos: SourcePosition
  ) extends MessageContainer(msgFn, pos, ERROR)

  class Warning(
    msgFn: => Message,
    pos: SourcePosition
  ) extends MessageContainer(msgFn, pos, WARNING)

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
  abstract class EmptyCatchOrFinallyBlock(tryBody: untpd.Tree, errNo: Int)(implicit ctx: Context)
  extends Message(errNo) {
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
  extends EmptyCatchOrFinallyBlock(tryBody, 1) {
    val kind = "Syntax"
    val msg =
      hl"""|The ${"catch"} block does not contain a valid expression, try
           |adding a case like - `${"case e: Exception =>"}` to the block"""
  }

  case class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody, 2) {
    val kind = "Syntax"
    val msg =
      hl"""|A ${"try"} without ${"catch"} or ${"finally"} is equivalent to putting
           |its body in a block; no exceptions are handled."""
  }

  case class DeprecatedWithOperator()(implicit ctx: Context)
  extends Message(3) {
    val kind = "Syntax"
    val msg =
      hl"""${"with"} as a type operator has been deprecated; use `&' instead"""
    val explanation =
      hl"""|Dotty introduces intersection types - `&' types. These replace the
           |use of the ${"with"} keyword. There are a few differences in
           |semantics between intersection types and using `${"with"}'."""
  }

  case class CaseClassMissingParamList(cdef: untpd.TypeDef)(implicit ctx: Context)
  extends Message(4) {
    val kind = "Syntax"
    val msg =
      hl"""|A ${"case class"} must have at least one parameter list"""

    val explanation =
      hl"""|${cdef.name} must have at least one parameter list, if you would rather
           |have a singleton representation of ${cdef.name}, use a "${"case object"}".
           |Or, add an explicit `()' as a parameter list to ${cdef.name}."""
  }


  // Type Errors ------------------------------------------------------------ //
  case class DuplicateBind(bind: untpd.Bind, tree: untpd.CaseDef)(implicit ctx: Context)
  extends Message(5) {
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

      hl"""|For each ${"case"} bound variable names  have to be unique. In:
           |
           |$caseDef
           |
           |`${bind.name}` is not unique. Rename one of the bound variables!"""
    }
  }

  case class MissingIdent(tree: untpd.Ident, treeKind: String, name: String)(implicit ctx: Context)
  extends Message(6) {
    val kind = "Missing Identifier"
    val msg = em"not found: $treeKind$name"

    val explanation = {
      hl"""|An identifier for `$treeKind$name` is missing. This means that something
           |has either been misspelt or you're forgetting an import"""
    }
  }

  case class TypeMismatch(found: Type, expected: Type, whyNoMatch: String = "", implicitFailure: String = "")(implicit ctx: Context)
  extends Message(7) {
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
  extends Message(8) {
    val kind = "Member Not Found"

    val msg = {
      import core.Flags._
      val maxDist = 3
      val decls = site.decls.flatMap { sym =>
        if (sym.is(Synthetic | PrivateOrLocal) || sym.isConstructor) Nil
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

      ex"$selected `$name` is not a member of $site$closeMember"
    }

    val explanation = ""
  }

  case class EarlyDefinitionsNotSupported()(implicit ctx:Context)
  extends Message(9) {
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
  extends Message(10) {
    val kind = "Syntax"
    val msg = hl"""An ${"implicit class"} may not be top-level"""

    val explanation = {
      val TypeDef(name, impl @ Template(constr0, parents, self, _)) = cdef
      val exampleArgs =
        constr0.vparamss(0).map(_.withMods(untpd.Modifiers()).show).mkString(", ")
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
  extends Message(11) {
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
  extends Message(12) {
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
  extends Message(13) {
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
  extends Message(14) {
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
  extends Message(15) {
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
  extends Message(16) {
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
           |but an uninitialized var definition"""
  }

  case class IllegalStartSimpleExpr(illegalToken: String)(implicit ctx: Context)
  extends Message(17) {
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

  case class MissingReturnType()(implicit ctx:Context) extends Message(18) {
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
  extends Message(19) {
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
  extends Message(20) {
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
  extends Message(21) {
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

  case class WrongNumberOfArgs(fntpe: Type, argKind: String, expectedArgs: List[TypeParamInfo], actual: List[untpd.Tree])(implicit ctx: Context)
  extends Message(22) {
    val kind = "Syntax"

    private val expectedCount = expectedArgs.length
    private val actualCount = actual.length
    private val msgPrefix = if (actualCount > expectedCount) "Too many" else "Not enough"

    //TODO add def simpleParamName to TypeParamInfo
    private val expectedArgString = fntpe
      .widen.typeParams
      .map(_.paramName.unexpandedName.show)
      .mkString("[", ", ", "]")

    private val actualArgString = actual.map(_.show).mkString("[", ", ", "]")

    private val prettyName = fntpe.termSymbol match {
      case NoSymbol => fntpe.show
      case symbol   => symbol.showFullName
    }

    val msg =
      hl"""|${NoColor(msgPrefix)} ${argKind} arguments for $prettyName$expectedArgString
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
  extends Message(23) {
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

  case class TypeParamsTypeExpected(mods: untpd.Modifiers, identifier: TermName)(implicit ctx: Context)
  extends Message(24) {
    val kind = "Syntax"
    val msg = hl"""Expected ${"type"} keyword for type parameter $identifier"""
    val explanation =
      hl"""|This happens when you add modifiers like ${"private"} or ${"protected"}
           |to your type parameter definition without adding the ${"type"} keyword.
           |
           |Add ${"type"} to your code, e.g.:
           |${s"trait A[${mods.flags} type $identifier]"}
           |"""
  }

  case class IdentifierExpected(identifier: String)(implicit ctx: Context)
  extends Message(25) {
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
  extends Message(26) {
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

  case class IncorrectRepeatedParameterSyntax()(implicit ctx: Context) extends Message(27) {
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
}
