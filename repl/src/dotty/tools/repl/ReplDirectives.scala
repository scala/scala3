package dotty.tools.repl

import scala.util.control.NonFatal

import dotty.tools.directives.{DirectiveValue, UsingDirectivesParser}

private[repl] object ReplDirectives:

  type Dependencies = List[String]

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

  case class DirectiveClassification(
    dependencies: Dependencies,
    warnings: List[Warning],
    hasDirectives: Boolean
  )

  private trait DirectiveHandler:
    def keys: List[String]
    def usage: String
    def description: String
    def process(values: Seq[DirectiveValue]): Dependencies
    def warnings: List[Warning] = Nil

    final def helpText: String =
      val aliasText = keys.drop(1) match
        case Nil => Nil
        case aliases => List(s"  Aliases: ${aliases.mkString(", ")}")
      (List(usage, s"  $description") ++ aliasText).mkString("\n")

  private object DependencyDirective extends DirectiveHandler:
    val keys = List("dep", "deps", "dependency", "dependencies")
    val usage = "//> using dep <group>::<artifact>:<version> ..."
    val description = "Resolve dependencies and make them available in the REPL."

    def process(values: Seq[DirectiveValue]): Dependencies =
      values.collect:
          case DirectiveValue.StringVal(value, _, _) => value
        .toList

  private object TestDependencyDirective extends DirectiveHandler:
    val keys = List("test.dep", "test.deps", "test.dependency", "test.dependencies")
    val usage = "//> using test.dep <group>::<artifact>:<version> ..."
    val description = "Resolve dependencies and make them available in the REPL."
    override val warnings = List(Warning.NoSeparateTestScope)

    def process(values: Seq[DirectiveValue]): Dependencies =
      DependencyDirective.process(values)

  private val handlers = List(DependencyDirective, TestDependencyDirective)
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
      val dependencies = supported
        .flatMap(directive => handlersByKey(directive.key).process(directive.values))
        .toList
      val warnings = (supported.flatMap(directive => handlersByKey(directive.key).warnings) ++
        unsupported.map(directive => Warning.UnsupportedDirective(directive.key)))
        .distinct
        .toList
      DirectiveClassification(dependencies, warnings, result.directives.nonEmpty)
    catch
      case NonFatal(_) => DirectiveClassification(Nil, Nil, false)
