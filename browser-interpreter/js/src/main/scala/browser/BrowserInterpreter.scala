package browser

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray._

import interpreter._
import tasty._

/**
 * JavaScript interface for the browser interpreter.
 *
 * This provides a clean API for calling the interpreter from JavaScript.
 */
@JSExportTopLevel("ScalaInterpreter")
object BrowserInterpreter {

  private val interpreter = new Interpreter()

  /**
   * Interpret a JSON AST and return the result.
   *
   * @param jsonAst JSON string representing the AST
   * @return JavaScript object with result information
   */
  @JSExport
  def interpret(jsonAst: String): js.Dynamic = {
    try {
      val ast = JsonParser.parse(jsonAst)
      val result = interpreter.interpret(ast)

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
      case e: Exception =>
        js.Dynamic.literal(
          success = false,
          output = interpreter.getOutput,
          result = null,
          error = s"Parse error: ${e.getMessage}",
          stats = js.Dynamic.literal(nodes = 0, calls = 0)
        )
    }
  }

  /**
   * Interpret a TASTy file directly.
   *
   * @param tastyBytes The TASTy file bytes as a JavaScript Uint8Array
   * @return JavaScript object with result information
   */
  @JSExport
  def interpretTasty(tastyBytes: Int8Array): js.Dynamic = {
    try {
      // Convert JS Int8Array to Scala Array[Byte]
      val bytes = new Array[Byte](tastyBytes.length)
      var i = 0
      while (i < tastyBytes.length) {
        bytes(i) = tastyBytes(i)
        i += 1
      }

      // Parse TASTy
      val unpickler = new TastyUnpickler(bytes)
      if (!unpickler.read()) {
        return js.Dynamic.literal(
          success = false,
          output = "",
          result = null,
          error = "Failed to read TASTy file",
          stats = js.Dynamic.literal(nodes = 0, calls = 0)
        )
      }

      // Unpickle AST
      val astUnpickler = new TastyAstUnpickler(unpickler)
      astUnpickler.unpickleMain() match {
        case Some(ast) =>
          val result = interpreter.interpret(ast)
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
        case None =>
          js.Dynamic.literal(
            success = false,
            output = "",
            result = null,
            error = "No main method found in TASTy file",
            stats = js.Dynamic.literal(nodes = 0, calls = 0)
          )
      }
    } catch {
      case e: Exception =>
        js.Dynamic.literal(
          success = false,
          output = "",
          result = null,
          error = s"TASTy interpretation error: ${e.getMessage}",
          stats = js.Dynamic.literal(nodes = 0, calls = 0)
        )
    }
  }

  /**
   * Read TASTy file info (header, sections, names).
   *
   * @param tastyBytes The TASTy file bytes
   * @return JavaScript object with TASTy file info
   */
  @JSExport
  def readTastyInfo(tastyBytes: Int8Array): js.Dynamic = {
    try {
      val bytes = new Array[Byte](tastyBytes.length)
      var i = 0
      while (i < tastyBytes.length) {
        bytes(i) = tastyBytes(i)
        i += 1
      }

      val unpickler = new TastyUnpickler(bytes)
      if (!unpickler.read()) {
        return js.Dynamic.literal(
          success = false,
          error = "Failed to read TASTy file"
        )
      }

      val header = unpickler.header.get
      js.Dynamic.literal(
        success = true,
        version = s"${header.majorVersion}.${header.minorVersion}.${header.experimentalVersion}",
        tooling = header.toolingVersion,
        sections = js.Array(unpickler.getSectionNames.map(js.Any.fromString)*)
      )
    } catch {
      case e: Exception =>
        js.Dynamic.literal(
          success = false,
          error = e.getMessage
        )
    }
  }

  /**
   * Get captured output.
   */
  @JSExport
  def getOutput(): String = interpreter.getOutput

  /**
   * Clear output buffer.
   */
  @JSExport
  def clearOutput(): Unit = interpreter.clearOutput()

  /**
   * Version information.
   */
  @JSExport
  val version: String = "0.2.0"

  /**
   * Quick test to verify the interpreter works.
   */
  @JSExport
  def test(): String = {
    val testAst = """
      {
        "tag": "Apply",
        "fn": {"tag": "Ident", "name": "println"},
        "args": [{"tag": "Literal", "type": "String", "value": "Hello from Scala.js!"}]
      }
    """
    val result = interpret(testAst)
    result.output.asInstanceOf[String]
  }
}
