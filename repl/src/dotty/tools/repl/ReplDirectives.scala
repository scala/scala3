package dotty.tools.repl

import scala.util.control.NonFatal

import dotty.tools.directives.{DirectiveValue, UsingDirectivesParser}

private[repl] object ReplDirectives:

  private def usageHint(key: String): String =
    handlersByKey.get(key).fold("")(handler => s"\nUsage: ${handler.usage}")

  enum Warning:
    case NoSeparateTestScope
    case TestToolkitSameAsToolkit
    case ValueMissing(key: String)
    case TooManyValues(key: String)
    case UnsupportedDirective(key: String)

    override def toString: String = this match
      case NoSeparateTestScope =>
        """[warn] The REPL does not have a separate test scope. Dependencies that would only be
          |available to tests are added to the current REPL session.""".stripMargin
      case TestToolkitSameAsToolkit =>
        """[warn] The REPL does not have a separate test scope, so `using test.toolkit` adds
          |exactly what `using toolkit` does.""".stripMargin
      case ValueMissing(key) =>
        s"[warn] The `using $key` directive was given no value. It was ignored.${usageHint(key)}"
      case TooManyValues(key) =>
        s"[warn] The `using $key` directive expects a single value. It was ignored.${usageHint(key)}"
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

  private def resolveVersion(rawVersion: String, default: String): String = rawVersion match
    case "default" => default
    case "latest" => "latest.release"
    case explicit => explicit

  private sealed trait Toolkit:
    val org: String
    val defaultVersion: String
    protected def matchesFlavor(flavor: String): Boolean
    def unapply(flavorAndVersion: (String, String)): Option[(String, String)] =
      val (flavor, rawVersion) = flavorAndVersion
      Option.when(matchesFlavor(flavor))((org, resolveVersion(rawVersion, defaultVersion)))

  private object ScalaToolkit extends Toolkit:
    val org = "org.scala-lang"
    val defaultVersion = "0.9.2"
    protected def matchesFlavor(flavor: String) = flavor == "scala" || flavor == org

  private object TypelevelToolkit extends Toolkit:
    val org = "org.typelevel"
    val defaultVersion = "0.2.0"
    protected def matchesFlavor(flavor: String) = flavor == "typelevel" || flavor == org

  def toolkitCoordinates(coords: String): List[String] =
    val tokens = coords.split(':').toList
    val rawVersion = tokens.lastOption.filter(_.nonEmpty).getOrElse("default")
    val flavor = tokens.dropRight(1).headOption.getOrElse("scala")
    val (org, version) = (flavor, rawVersion) match
      case TypelevelToolkit(org, version) => (org, version)
      case ScalaToolkit(org, version) => (org, version)
      case (customOrg, rawVersion) => (customOrg, resolveVersion(rawVersion, default = rawVersion))
    List("toolkit", "toolkit-test").map(artifact => s"$org::$artifact:$version")

  private def toolkitDependencies(coords: String): List[ReplDirective] =
    toolkitCoordinates(coords).map(ReplDirective.Dependency(_))

  private enum DirectiveHandler(
    val keys: List[String],
    val usage: String,
    val description: String,
    toDirectives: String => List[ReplDirective],
    val warnings: List[Warning] = Nil,
    val acceptsMultipleValues: Boolean = true
  ):
    case Dependency extends DirectiveHandler(
      keys = List("dep", "deps", "dependency", "dependencies"),
      usage = "//> using dep <group>::<artifact>:<version> ...",
      description = "Resolve dependencies and make them available in the REPL.",
      toDirectives = coords => List(ReplDirective.Dependency(coords))
    )

    case TestDependency extends DirectiveHandler(
      keys = List("test.dep", "test.deps", "test.dependency", "test.dependencies"),
      usage = "//> using test.dep <group>::<artifact>:<version> ...",
      description = "Resolve dependencies and make them available in the REPL.",
      toDirectives = coords => List(ReplDirective.Dependency(coords)),
      warnings = List(Warning.NoSeparateTestScope)
    )

    case Jar extends DirectiveHandler(
      keys = List("jar", "jars"),
      usage = "//> using jar <path> ...",
      description = "Add JARs to the REPL classpath.",
      toDirectives = path => List(ReplDirective.Jar(path))
    )

    case Toolkit extends DirectiveHandler(
      keys = List("toolkit"),
      usage = "//> using toolkit <version>|default|<flavor>:<version>",
      description =
        s"""Resolve a toolkit, along with its test artifact, and make it available in the REPL.
           |  Known flavors: scala (default, ${ScalaToolkit.defaultVersion}),
           |  typelevel (${TypelevelToolkit.defaultVersion}).""".stripMargin,
      toDirectives = toolkitDependencies,
      warnings = List(Warning.NoSeparateTestScope),
      acceptsMultipleValues = false
    )

    case TestToolkit extends DirectiveHandler(
      keys = List("test.toolkit"),
      usage = "//> using test.toolkit <version>|default|<flavor>:<version>",
      description = "Resolve a toolkit along with its test artifact and make them available in the REPL.",
      toDirectives = toolkitDependencies,
      warnings = List(Warning.TestToolkitSameAsToolkit),
      acceptsMultipleValues = false
    )

    def process(values: Seq[DirectiveValue]): List[ReplDirective] =
      values.collect:
          case DirectiveValue.StringVal(value, _, _) => toDirectives(value)
        .toList.flatten

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

  private def carriesNoValue(value: DirectiveValue): Boolean = value match
    case DirectiveValue.EmptyVal(_) => true
    case _ => false

  def classify(sourceCode: String): DirectiveClassification =
    try
      val result = UsingDirectivesParser.parse(sourceCode)
      val (supported, unsupported) = result.directives.partition(directive => handlersByKey.contains(directive.key))
      val (valueless, valued) = supported.partition(_.values.forall(carriesNoValue))
      val (overfull, accepted) = valued.partition: directive =>
        !handlersByKey(directive.key).acceptsMultipleValues && directive.values.sizeIs > 1
      val directives = accepted
        .flatMap(directive => handlersByKey(directive.key).process(directive.values))
        .toList
      val warnings = (accepted.flatMap(directive => handlersByKey(directive.key).warnings) ++
        valueless.map(directive => Warning.ValueMissing(directive.key)) ++
        overfull.map(directive => Warning.TooManyValues(directive.key)) ++
        unsupported.map(directive => Warning.UnsupportedDirective(directive.key)))
        .distinct
        .toList
      DirectiveClassification(directives, warnings, result.directives.nonEmpty)
    catch
      case NonFatal(_) => DirectiveClassification(Nil, Nil, false)
