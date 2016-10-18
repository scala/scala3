package dotty.tools
package dotc
package reporting
package diagnostic

import dotc.core._
import Contexts.Context, Decorators._, Symbols._, Names._, Types._
import ast.untpd.{Modifiers, ModuleDef}
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
  import dotc.ast.Trees._
  import dotc.ast.untpd

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
           |correctly handles transfer functions like ${"return"}.""".stripMargin
    }
  }

  case class EmptyCatchBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody, 1) {
    val kind = "Syntax"
    val msg =
      hl"""|The ${"catch"} block does not contain a valid expression, try
           |adding a case like - `${"case e: Exception =>"}` to the block""".stripMargin
  }

  case class EmptyCatchAndFinallyBlock(tryBody: untpd.Tree)(implicit ctx: Context)
  extends EmptyCatchOrFinallyBlock(tryBody, 2) {
    val kind = "Syntax"
    val msg =
      hl"""|A ${"try"} without ${"catch"} or ${"finally"} is equivalent to putting
           |its body in a block; no exceptions are handled.""".stripMargin
  }

  case class DeprecatedWithOperator()(implicit ctx: Context)
  extends Message(3) {
    val kind = "Syntax"
    val msg =
      hl"""${"with"} as a type operator has been deprecated; use `&' instead"""
    val explanation =
      hl"""|Dotty introduces intersection types - `&' types. These replace the
           |use of the ${"with"} keyword. There are a few differences in
           |semantics between intersection types and using `${"with"}'.""".stripMargin
  }

  case class CaseClassMissingParamList(cdef: untpd.TypeDef)(implicit ctx: Context)
  extends Message(4) {
    val kind = "Syntax"
    val msg =
      hl"""|A ${"case class"} must have at least one parameter list"""

    val explanation =
      hl"""|${cdef.name} must have at least one parameter list, if you would rather
           |have a singleton representation of ${cdef.name}, use a "${"case object"}".
           |Or, add an explicit `()' as a parameter list to ${cdef.name}.""".stripMargin
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
           |`${bind.name}` is not unique. Rename one of the bound variables!""".stripMargin
    }
  }

  case class MissingIdent(tree: untpd.Ident, treeKind: String, name: String)(implicit ctx: Context)
  extends Message(6) {
    val kind = "Missing Identifier"
    val msg = em"not found: $treeKind$name"

    val explanation = {
      hl"""|An identifier for `$treeKind$name` is missing. This means that something
           |has either been misspelt or you're forgetting an import""".stripMargin
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
 
  case class EarlyDefinitionsNotSupported()(implicit ctx:Context) extends Message(9) {
    val kind = "Syntax"
   
    val msg = "early definitions are not supported; use trait parameters instead"

    val code1 =
      """|trait Logging {
         |  val f: File
         |  f.open()
         |  onExit(f.close())
         |  def log(msg: String) = f.write(msg)
         |}
         |
         |class B extends Logging {
         |  val f = new File("log.data") // triggers a null pointer exception
         |}
         |
         |class C extends {
         |  val f = new File("log.data") // early definition gets around the null pointer exception
         |} with Logging""".stripMargin

    val code2 =
      """|trait Logging(f: File) {
         |  f.open()
         |  onExit(f.close())
         |  def log(msg: String) = f.write(msg)
         |}
         |
         |class C extends Logging(new File("log.data"))""".stripMargin

    val explanation =
      hl"""Earlier versions of Scala did not support trait parameters and "early definitions" (also known as "early initializers")
        |were used as an alternative.
        |
        |Example of old syntax:
        |
        |$code1
        |
        |The above code can now be written as:
        |
        |$code2
        |""".stripMargin
  }

  def implicitClassRestrictionsText(implicit ctx: Context) =
    hl"""${NoColor("For a full list of restrictions on implicit classes visit")}
      |  ${Blue("http://docs.scala-lang.org/overviews/core/implicit-classes.html")}""".stripMargin

  case class TopLevelImplicitClass(cdef: untpd.TypeDef)(implicit ctx: Context)
    extends Message(10) {
    val kind = "Syntax"

    val msg = hl"""|An ${"implicit class"} may not be top-level"""

    val explanation = {
      val TypeDef(name, impl @ Template(constr0, parents, self, _)) = cdef
      val exampleArgs = constr0.vparamss(0).map(_.withMods(Modifiers()).show).mkString(", ")
      def defHasBody[T] = impl.body.exists(!_.isEmpty)
      val exampleBody = if (defHasBody) "{\n ...\n }" else ""
      hl"""|There may not be any method, member or object in scope with the same name as the
           |implicit class and a case class automatically gets a companion object with the same name
           |created by the compiler which would cause a naming conflict if it were allowed.
           |
           |""".stripMargin + implicitClassRestrictionsText + hl"""|
           |
           |To resolve the conflict declare ${cdef.name} inside of an ${"object"} then import the class
           |from the object at the use site if needed, for example:
           |
           |object Implicits {
           |  implicit class ${cdef.name}($exampleArgs)$exampleBody
           |}
           |
           |// At the use site:
           |import Implicits.${cdef.name}""".stripMargin
    }
  }

  case class ImplicitCaseClass(cdef: untpd.TypeDef)(implicit ctx: Context)
    extends Message(11) {
    val kind = "Syntax"

    val msg = hl"""|A ${"case class"} may not be defined as ${"implicit"}"""

    val explanation =
      hl"""|implicit classes may not be case classes. Instead use a plain class:
           |  example: implicit class ${cdef.name}...
           |
           |""".stripMargin + implicitClassRestrictionsText
  }

  case class ObjectMayNotHaveSelfType(mdef: untpd.ModuleDef)(implicit ctx: Context)
    extends Message(12) {
    val kind = "Syntax"

    val msg = hl"""|${"objects"} must not have a ${"self type"}"""

    val explanation = {
      val ModuleDef(name, tmpl) = mdef
      val ValDef(_, selfTpt, _) = tmpl.self
      hl"""|objects must not have a ${"self type"}:
           |
           |Consider these alternative solutions:
           |  - Create a trait or a class instead of an object
           |  - Let the object extend a trait containing the self type:
           |      example: object $name extends ${selfTpt.show}""".stripMargin
    }
  }

  case class TupleTooLong(ts: List[untpd.Tree])(implicit ctx: Context)
    extends Message(13) {
    import Definitions.MaxTupleArity
    val kind = "Syntax"

    val msg = hl"""|A ${"tuple"} cannot have more than ${MaxTupleArity} members"""

    val explanation = {
      val members = ts.map(_.showSummary).grouped(MaxTupleArity)
      val nestedRepresentation = members.map(_.mkString(", ")).mkString(")(")
      hl"""|This restriction will be removed in the future.
           |Currently it is possible to use nested tuples when more than ${MaxTupleArity} are needed, for example:
           |
           |  ((${nestedRepresentation}))""".stripMargin
    }
  }
}
