package browser

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.util.control.NonFatal

import dotc.core.Names._
import dotc.util.SourceFile
import dotc.parsing.{Scanners, Parser}
import dotc.ast.Trees._
import interpreter.{Interpreter, AstConverter, Ast}

/**
 * Browser compiler API exposed to JavaScript.
 *
 * This provides a high-level interface for compiling Scala code
 * in the browser and producing TASTy output.
 */
@JSExportTopLevel("ScalaCompiler")
object BrowserCompiler {

  /** Compiler version */
  @JSExport
  def version(): String = "0.1.0-browser"

  /** Compile Scala source code to TASTy bytes */
  @JSExport
  def compile(source: String): js.Dynamic = {
    try {
      val result = compileInternal(source)
      js.Dynamic.literal(
        success = result.success,
        tastyBytes = if (result.success) js.Array(result.tastyBytes.map(_.toInt)*) else js.Array[Int](),
        errors = js.Array(result.errors.map(js.Any.fromString)*),
        warnings = js.Array(result.warnings.map(js.Any.fromString)*)
      )
    } catch {
      case NonFatal(e) =>
        js.Dynamic.literal(
          success = false,
          tastyBytes = js.Array[Int](),
          errors = js.Array(s"Compiler error: ${e.getMessage}"),
          warnings = js.Array[String]()
        )
    }
  }

  /** Parse Scala source code and return AST as JSON */
  @JSExport
  def parse(source: String): js.Dynamic = {
    try {
      val sourceFile = SourceFile("input.scala", source)
      val parser = new Parser(sourceFile)
      val trees = parser.parse()
      val errors = parser.getErrors

      if (errors.nonEmpty) {
        js.Dynamic.literal(
          success = false,
          ast = null,
          errors = js.Array(errors.map(js.Any.fromString)*)
        )
      } else {
        js.Dynamic.literal(
          success = true,
          ast = treeToJson(trees),
          astString = trees.map(_.toString).mkString("\n"),
          errors = js.Array[String]()
        )
      }
    } catch {
      case NonFatal(e) =>
        js.Dynamic.literal(
          success = false,
          ast = null,
          errors = js.Array(s"Parse error: ${e.getMessage}\n${e.getStackTrace.take(5).mkString("\n")}")
        )
    }
  }

  /** Tokenize Scala source code */
  @JSExport
  def tokenize(source: String): js.Dynamic = {
    try {
      val sourceFile = SourceFile("input.scala", source)
      val scanner = new Scanners.Scanner(sourceFile)

      val tokens = js.Array[js.Dynamic]()
      while (scanner.token != dotc.parsing.Tokens.EOF) {
        tokens.push(js.Dynamic.literal(
          token = scanner.token,
          tokenStr = dotc.parsing.Tokens.showToken(scanner.token),
          offset = scanner.offset,
          name = if (scanner.name != null) scanner.name.toString else null,
          strVal = scanner.strVal
        ))
        scanner.nextToken()
      }

      js.Dynamic.literal(
        success = true,
        tokens = tokens,
        errors = js.Array(scanner.getErrors.map { case (msg, off) => s"$off: $msg" }.map(js.Any.fromString)*)
      )
    } catch {
      case NonFatal(e) =>
        js.Dynamic.literal(
          success = false,
          tokens = js.Array[js.Dynamic](),
          errors = js.Array(s"Tokenize error: ${e.getMessage}")
        )
    }
  }

  private def treeToJson(trees: List[Tree]): js.Array[js.Dynamic] = {
    js.Array(trees.map(singleTreeToJson)*)
  }

  private def singleTreeToJson(tree: Tree): js.Dynamic = tree match {
    case PackageDef(pid, stats) =>
      js.Dynamic.literal(
        kind = "PackageDef",
        pid = singleTreeToJson(pid),
        stats = js.Array(stats.map(singleTreeToJson)*)
      )
    case Import(expr, selectors) =>
      js.Dynamic.literal(
        kind = "Import",
        expr = singleTreeToJson(expr)
      )
    case ClassDef(name, tparams, template) =>
      js.Dynamic.literal(
        kind = "ClassDef",
        name = name.toString,
        tparams = js.Array(tparams.map(singleTreeToJson)*)
      )
    case ModuleDef(name, template) =>
      js.Dynamic.literal(
        kind = "ModuleDef",
        name = name.toString
      )
    case ValDef(name, tpt, rhs) =>
      js.Dynamic.literal(
        kind = "ValDef",
        name = name.toString,
        tpt = singleTreeToJson(tpt),
        rhs = singleTreeToJson(rhs)
      )
    case DefDef(name, paramss, tpt, rhs) =>
      js.Dynamic.literal(
        kind = "DefDef",
        name = name.toString,
        tpt = singleTreeToJson(tpt),
        rhs = singleTreeToJson(rhs)
      )
    case TypeDef(name, rhs) =>
      js.Dynamic.literal(
        kind = "TypeDef",
        name = name.toString,
        rhs = singleTreeToJson(rhs)
      )
    case Ident(name) =>
      js.Dynamic.literal(
        kind = "Ident",
        name = name.toString
      )
    case Select(qual, name) =>
      js.Dynamic.literal(
        kind = "Select",
        qualifier = singleTreeToJson(qual),
        name = name.toString
      )
    case Apply(fun, args) =>
      js.Dynamic.literal(
        kind = "Apply",
        fun = singleTreeToJson(fun),
        args = js.Array(args.map(singleTreeToJson)*)
      )
    case TypeApply(fun, args) =>
      js.Dynamic.literal(
        kind = "TypeApply",
        fun = singleTreeToJson(fun),
        args = js.Array(args.map(singleTreeToJson)*)
      )
    case Block(stats, expr) =>
      js.Dynamic.literal(
        kind = "Block",
        stats = js.Array(stats.map(singleTreeToJson)*),
        expr = singleTreeToJson(expr)
      )
    case If(cond, thenp, elsep) =>
      js.Dynamic.literal(
        kind = "If",
        cond = singleTreeToJson(cond),
        thenp = singleTreeToJson(thenp),
        elsep = singleTreeToJson(elsep)
      )
    case Match(selector, cases) =>
      js.Dynamic.literal(
        kind = "Match",
        selector = singleTreeToJson(selector),
        cases = js.Array(cases.map(singleTreeToJson)*)
      )
    case CaseDef(pat, guard, body) =>
      js.Dynamic.literal(
        kind = "CaseDef",
        pattern = singleTreeToJson(pat),
        guard = singleTreeToJson(guard),
        body = singleTreeToJson(body)
      )
    case Try(expr, cases, finalizer) =>
      js.Dynamic.literal(
        kind = "Try",
        expr = singleTreeToJson(expr),
        cases = js.Array(cases.map(singleTreeToJson)*),
        finalizer = singleTreeToJson(finalizer)
      )
    case Function(args, body) =>
      js.Dynamic.literal(
        kind = "Function",
        args = js.Array(args.map(singleTreeToJson)*),
        body = singleTreeToJson(body)
      )
    case Literal(const) =>
      js.Dynamic.literal(
        kind = "Literal",
        value = const.value match {
          case null => null
          case s: String => s
          case n: Number => n.doubleValue()
          case b: Boolean => b
          case c: Char => c.toString
          case () => "()"
          case x => x.toString
        }
      )
    case New(tpt) =>
      js.Dynamic.literal(
        kind = "New",
        tpt = singleTreeToJson(tpt)
      )
    case Typed(expr, tpt) =>
      js.Dynamic.literal(
        kind = "Typed",
        expr = singleTreeToJson(expr),
        tpt = singleTreeToJson(tpt)
      )
    case Assign(lhs, rhs) =>
      js.Dynamic.literal(
        kind = "Assign",
        lhs = singleTreeToJson(lhs),
        rhs = singleTreeToJson(rhs)
      )
    case InfixOp(left, op, right) =>
      js.Dynamic.literal(
        kind = "InfixOp",
        left = singleTreeToJson(left),
        op = singleTreeToJson(op),
        right = singleTreeToJson(right)
      )
    case PrefixOp(op, od) =>
      js.Dynamic.literal(
        kind = "PrefixOp",
        op = singleTreeToJson(op),
        operand = singleTreeToJson(od)
      )
    case Parens(t) =>
      js.Dynamic.literal(
        kind = "Parens",
        expr = singleTreeToJson(t)
      )
    case Tuple(trees) =>
      js.Dynamic.literal(
        kind = "Tuple",
        elements = js.Array(trees.map(singleTreeToJson)*)
      )
    case WhileDo(cond, body) =>
      js.Dynamic.literal(
        kind = "WhileDo",
        cond = singleTreeToJson(cond),
        body = singleTreeToJson(body)
      )
    case ForYield(enums, expr) =>
      js.Dynamic.literal(
        kind = "ForYield",
        enums = js.Array(enums.map(singleTreeToJson)*),
        expr = singleTreeToJson(expr)
      )
    case ForDo(enums, body) =>
      js.Dynamic.literal(
        kind = "ForDo",
        enums = js.Array(enums.map(singleTreeToJson)*),
        body = singleTreeToJson(body)
      )
    case Return(expr, _) =>
      js.Dynamic.literal(
        kind = "Return",
        expr = singleTreeToJson(expr)
      )
    case Throw(expr) =>
      js.Dynamic.literal(
        kind = "Throw",
        expr = singleTreeToJson(expr)
      )
    case AppliedTypeTree(tpt, args) =>
      js.Dynamic.literal(
        kind = "AppliedTypeTree",
        tpt = singleTreeToJson(tpt),
        args = js.Array(args.map(singleTreeToJson)*)
      )
    case TypeBoundsTree(lo, hi) =>
      js.Dynamic.literal(
        kind = "TypeBoundsTree",
        lo = singleTreeToJson(lo),
        hi = singleTreeToJson(hi)
      )
    case EmptyTree =>
      js.Dynamic.literal(kind = "EmptyTree")
    case _ =>
      js.Dynamic.literal(
        kind = tree.getClass.getSimpleName,
        toString = tree.toString
      )
  }

  /** Check syntax of Scala source code */
  @JSExport
  def checkSyntax(source: String): js.Dynamic = {
    try {
      val sourceFile = SourceFile("input.scala", source)
      val scanner = new Scanners.Scanner(sourceFile)

      // Scan all tokens to find syntax errors
      while (scanner.token != dotc.parsing.Tokens.EOF) {
        scanner.nextToken()
      }

      val errors = scanner.getErrors
      js.Dynamic.literal(
        valid = errors.isEmpty,
        errors = js.Array(errors.map { case (msg, off) => s"$off: $msg" }.map(js.Any.fromString)*)
      )
    } catch {
      case NonFatal(e) =>
        js.Dynamic.literal(
          valid = false,
          errors = js.Array(s"Syntax check error: ${e.getMessage}")
        )
    }
  }

  /** Get list of supported features */
  @JSExport
  def supportedFeatures(): js.Array[String] = js.Array(
    "Basic types (Int, Long, Float, Double, Boolean, Char, String)",
    "Functions and methods",
    "Classes and objects",
    "Pattern matching (basic)",
    "Exception handling",
    "Generics (basic)",
    "Packages and imports"
  )

  /** Get list of unsupported features */
  @JSExport
  def unsupportedFeatures(): js.Array[String] = js.Array(
    "Macros",
    "Java interop",
    "Reflection",
    "File I/O",
    "Network I/O",
    "Incremental compilation",
    "Parallel compilation"
  )

  /** Parse, convert, and run Scala code */
  @JSExport
  def run(source: String): js.Dynamic = {
    try {
      // Parse the source
      val sourceFile = SourceFile("input.scala", source)
      val parser = new Parser(sourceFile)
      val trees = parser.parse()
      val parseErrors = parser.getErrors

      if (parseErrors.nonEmpty) {
        return js.Dynamic.literal(
          success = false,
          output = "",
          result = null,
          error = s"Parse errors:\n${parseErrors.mkString("\n")}",
          parseErrors = js.Array(parseErrors.map(js.Any.fromString)*)
        )
      }

      // Convert parser AST to interpreter AST
      val interpreterAst = AstConverter.convert(trees)

      if (interpreterAst.isEmpty) {
        return js.Dynamic.literal(
          success = true,
          output = "",
          result = "()",
          error = null
        )
      }

      // Create a block with all statements
      val program = if (interpreterAst.size == 1) {
        interpreterAst.head
      } else {
        Ast.Block(interpreterAst.init, interpreterAst.last)
      }

      // Run the interpreter
      val interpreter = new Interpreter()
      val result = interpreter.interpret(program)

      js.Dynamic.literal(
        success = result.success,
        output = result.output,
        result = result.result.getOrElse(null),
        error = result.error.getOrElse(null),
        stats = js.Dynamic.literal(
          nodes = result.stats.nodes,
          calls = result.stats.calls
        )
      )
    } catch {
      case NonFatal(e) =>
        js.Dynamic.literal(
          success = false,
          output = "",
          result = null,
          error = s"Runtime error: ${e.getMessage}\n${e.getStackTrace.take(5).mkString("\n")}"
        )
    }
  }

  // Internal compilation implementation
  private case class CompilationResult(
    success: Boolean,
    tastyBytes: Array[Byte],
    errors: List[String],
    warnings: List[String]
  )

  private def compileInternal(source: String): CompilationResult = {
    // Create source file
    val sourceFile = SourceFile("input.scala", source)

    // First, scan tokens to check for lexical errors
    val scanner = new Scanners.Scanner(sourceFile)
    val scannerErrors = {
      while (scanner.token != dotc.parsing.Tokens.EOF) {
        scanner.nextToken()
      }
      scanner.getErrors.map { case (msg, off) => s"Line ${sourceFile.offsetToLine(off) + 1}: $msg" }
    }

    if (scannerErrors.nonEmpty) {
      return CompilationResult(
        success = false,
        tastyBytes = Array.empty,
        errors = scannerErrors,
        warnings = Nil
      )
    }

    // Parse the source code
    val parser = new Parser(sourceFile)
    val trees = parser.parse()
    val parseErrors = parser.getErrors

    if (parseErrors.nonEmpty) {
      return CompilationResult(
        success = false,
        tastyBytes = Array.empty,
        errors = parseErrors,
        warnings = Nil
      )
    }

    // For now, return success with parse info
    // Full type checking and TASTy generation would go here
    CompilationResult(
      success = true,
      tastyBytes = Array.empty,
      errors = Nil,
      warnings = List(s"Parsed ${trees.size} top-level definition(s). Type checking and TASTy generation not yet implemented.")
    )
  }
}

