package dotty.tools.repl

import scala.util.control.NonFatal

import dotty.tools.directives.{DirectiveValue, UsingDirective, UsingDirectivesParser}

private[repl] object ReplDirectives:

  private def usageHint(key: String): String =
    handlersByKey.get(key).fold("")(handler => s"\nUsage: ${handler.usage}")

  enum Warning:
    case NoSeparateTestScope
    case TestToolkitSameAsToolkit
    case ValueMissing(key: String)
    case TooManyValues(key: String)
    case MalformedValue(key: String, value: String)
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
      case MalformedValue(key, value) =>
        s"[warn] The `using $key` directive does not recognize `$value`. It was ignored.${usageHint(key)}"
      case UnsupportedDirective(key) =>
        s"""[warn] The `using $key` directive is not supported in the REPL.
           |To use it, re-run with the `scala` command and pass the directive inside an input.""".stripMargin

  enum ReplDirective:
    case Dependency(coordinate: String)
    case Jar(path: String)
    case Repository(repository: String)

  case class DirectiveClassification(
    directives: List[ReplDirective],
    warnings: List[Warning],
    hasDirectives: Boolean
  )

  private def resolveVersion(rawVersion: String, default: String): String = rawVersion match
    case "default" => default
    case "latest" => "latest.release"
    case explicit => explicit

  private case class Toolkit(alias: String, org: String, defaultVersion: String)

  private val ScalaToolkit = Toolkit("scala", "org.scala-lang", "0.9.2")
  private val TypelevelToolkit = Toolkit("typelevel", "org.typelevel", "0.2.0")

  private val toolkitsByFlavor: Map[String, Toolkit] =
    List(ScalaToolkit, TypelevelToolkit)
      .flatMap(toolkit => List(toolkit.alias -> toolkit, toolkit.org -> toolkit))
      .toMap

  def toolkitCoordinates(coords: String): Option[List[String]] =
    val flavorAndVersion = coords.split(":", -1).toList match
      // e.g. //> using toolkit typelevel would otherwise be treated as version `typelevel`
      // and fail during resolution with a cryptic error, so we reject it early
      case version :: Nil if version.nonEmpty && !toolkitsByFlavor.keySet.contains(version) =>
        Some((ScalaToolkit.alias, version))
      case flavor :: version :: Nil if flavor.nonEmpty && version.nonEmpty => Some((flavor, version))
      case _ => None
    flavorAndVersion.map: (flavor, rawVersion) =>
      val (org, version) = toolkitsByFlavor.get(flavor) match
        case Some(toolkit) => (toolkit.org, resolveVersion(rawVersion, toolkit.defaultVersion))
        case None => (flavor, resolveVersion(rawVersion, default = rawVersion))
      List("toolkit", "toolkit-test").map(artifact => s"$org::$artifact:$version")

  private def toolkitDependencies(coords: String): List[ReplDirective] =
    toolkitCoordinates(coords).getOrElse(Nil).map(ReplDirective.Dependency(_))

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

    case Repository extends DirectiveHandler(
      keys = List("repository", "repositories"),
      usage = "//> using repository <url>|<alias> ...",
      description = "Add repositories, consulted before the default ones, to dependency resolution.",
      toDirectives = repository => List(ReplDirective.Repository(repository))
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

  private def classifyDirective(directive: UsingDirective): (List[ReplDirective], List[Warning]) =
    val UsingDirective(key, values, _) = directive
    handlersByKey.get(key) match
      case None => (Nil, List(Warning.UnsupportedDirective(key)))
      case Some(handler) =>
        if values.forall(carriesNoValue) then
          (Nil, List(Warning.ValueMissing(key)))
        else if !handler.acceptsMultipleValues && values.sizeIs > 1 then
          (Nil, List(Warning.TooManyValues(key)))
        else handler.process(values) match
          case Nil => (Nil, List(Warning.MalformedValue(key, values.map(_.stringValue).mkString(" "))))
          case directives => (directives, handler.warnings)

  def classify(sourceCode: String): DirectiveClassification =
    try
      val parsed = UsingDirectivesParser.parse(sourceCode).directives
      val (directives, warnings) = parsed.map(classifyDirective).unzip
      DirectiveClassification(directives.flatten.toList, warnings.flatten.distinct.toList, parsed.nonEmpty)
    catch
      case NonFatal(_) => DirectiveClassification(Nil, Nil, false)
