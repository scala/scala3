package dotty.tools.repl

import scala.util.control.NonFatal

import dotty.tools.directives.{DirectiveValue, UsingDirectivesParser}

private[repl] object ReplDirectives:

  enum Warning:
    case NoSeparateTestScope
    case UnsupportedDirective(key: String)

    override def toString: String = this match
      case NoSeparateTestScope =>
        """[warn] The REPL does not have a separate test scope. Dependencies declared with a
          |`using test.*` directive are added to the current REPL session.""".stripMargin
      case UnsupportedDirective(key) =>
        s"""[warn] The `using $key` directive is not supported in the REPL.
           |To use it, re-run with the `scala` command and pass the directive inside an input.""".stripMargin

  enum ReplDirective:
    case Dependency(coordinate: String)
    case Jar(path: String)

  case class DirectiveClassification(
    directives: List[ReplDirective],
    warnings: List[Warning],
    hasDirectives: Boolean
  )

  private enum DirectiveHandler(
    val keys: List[String],
    val usage: String,
    val description: String,
    toDirective: String => ReplDirective,
    val warnings: List[Warning] = Nil
  ):
    case Dependency extends DirectiveHandler(
      keys = List("dep", "deps", "dependency", "dependencies"),
      usage = "//> using dep <group>::<artifact>:<version> ...",
      description = "Resolve dependencies and make them available in the REPL.",
      toDirective = ReplDirective.Dependency(_)
    )

    case TestDependency extends DirectiveHandler(
      keys = List("test.dep", "test.deps", "test.dependency", "test.dependencies"),
      usage = "//> using test.dep <group>::<artifact>:<version> ...",
      description = "Resolve dependencies and make them available in the REPL.",
      toDirective = ReplDirective.Dependency(_),
      warnings = List(Warning.NoSeparateTestScope)
    )

    case Jar extends DirectiveHandler(
      keys = List("jar", "jars"),
      usage = "//> using jar <path> ...",
      description = "Add JARs to the REPL classpath.",
      toDirective = ReplDirective.Jar(_)
    )

    def process(values: Seq[DirectiveValue]): List[ReplDirective] =
      values.collect:
          case DirectiveValue.StringVal(value, _, _) => toDirective(value)
        .toList

    final def helpText: String =
      val aliasText = keys.drop(1) match
        case Nil => Nil
        case aliases => List(s"  Aliases: ${aliases.mkString(", ")}")
      (List(usage, s"  $description") ++ aliasText).mkString("\n")

  private val handlers = DirectiveHandler.values.toList
  private val handlersByKey = handlers.flatMap(handler => handler.keys.map(_ -> handler)).toMap

  require(
    handlersByKey.size == handlers.map(_.keys.size).sum,
    "REPL directive keys must be unique"
  )

  val helpText: String =
    s"""Supported `//> using` directives:
       |
       |${handlers.map(_.helpText).mkString("\n\n")}
       |
       |Directives must appear before any Scala code in the input; code on following lines is
       |evaluated as usual. Other `//> using` directives are not (yet) supported in the REPL.
       |""".stripMargin

  def classify(sourceCode: String): DirectiveClassification =
    try
      val result = UsingDirectivesParser.parse(sourceCode)
      val (supported, unsupported) = result.directives.partition(directive => handlersByKey.contains(directive.key))
      val directives = supported
        .flatMap(directive => handlersByKey(directive.key).process(directive.values))
        .toList
      val warnings = (supported.flatMap(directive => handlersByKey(directive.key).warnings) ++
        unsupported.map(directive => Warning.UnsupportedDirective(directive.key)))
        .distinct
        .toList
      DirectiveClassification(directives, warnings, result.directives.nonEmpty)
    catch
      case NonFatal(_) => DirectiveClassification(Nil, Nil, false)
