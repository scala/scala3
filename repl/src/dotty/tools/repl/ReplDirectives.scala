package dotty.tools.repl

import scala.util.control.NonFatal

import dotty.tools.directives.{DirectiveValue, UsingDirectivesParser}

private[repl] object ReplDirectives:

  type Dependencies = List[String]

  case class DirectiveClassification(
    dependencies: Dependencies,
    unsupportedKeys: List[String],
    hasDirectives: Boolean
  )

  private trait DirectiveHandler:
    def keys: List[String]
    def usage: String
    def description: String
    def process(values: Seq[DirectiveValue]): Dependencies

    final def helpText: String =
      val aliasText = keys.drop(1) match
        case Nil => Nil
        case aliases => List(s"  Aliases: ${aliases.mkString(", ")}")
      (List(usage, s"  $description") ++ aliasText).mkString("\n")

  private object DependencyDirective extends DirectiveHandler:
    val keys = List("dep")
    val usage = "//> using dep <group>::<artifact>:<version> ..."
    val description = "Resolve dependencies and make them available in the REPL."

    def process(values: Seq[DirectiveValue]): Dependencies =
      values.collect:
          case DirectiveValue.StringVal(value, _, _) => value
        .toList

  private val handlers = List(DependencyDirective)
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
      val unsupportedKeys = unsupported.map(_.key).distinct.toList
      DirectiveClassification(dependencies, unsupportedKeys, result.directives.nonEmpty)
    catch
      case NonFatal(_) => DirectiveClassification(Nil, Nil, false)
