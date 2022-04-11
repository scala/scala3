package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.{Reader, StringWriter}
import javax.script.{AbstractScriptEngine, Bindings, ScriptContext, ScriptEngine => JScriptEngine, ScriptEngineFactory, ScriptException, SimpleBindings}
import dotc.core.StdNames.str

/** A JSR 223 (Scripting API) compatible wrapper around the REPL for improved
 *  interoperability with software that supports it.
 *
 *  It works by instantiating a new script engine through the script engine manager.
 *  The script engine provides a eval method to evaluate scripts in string form.
 *  Example use:
 *
 *  val m = new javax.script.ScriptEngineManager()
 *  val e = m.getEngineByName("scala")
 *  println(e.eval("42"))
 */
class ScriptEngine extends AbstractScriptEngine {
  private val driver = new ReplDriver(
    Array(
      "-classpath", "", // Avoid the default "."
      "-usejavacp",
      "-color:never",
      "-Xrepl-disable-display"
    ), Console.out, None)
  private val rendering = new Rendering(Some(getClass.getClassLoader))
  private var state: State = driver.initialState

  def getFactory: ScriptEngineFactory = new ScriptEngine.Factory

  def createBindings: Bindings = new SimpleBindings

  /* Evaluate with the given context. */
  @throws[ScriptException]
  def eval(script: String, context: ScriptContext): Object = {
    val vid = state.valIndex
    state = driver.run(script)(state)
    val oid = state.objectIndex
    Class.forName(s"${Rendering.REPL_WRAPPER_NAME_PREFIX}$oid", true, rendering.classLoader()(using state.context))
      .getDeclaredMethods.find(_.getName == s"${str.REPL_RES_PREFIX}$vid")
      .map(_.invoke(null))
      .getOrElse(null)
  }

  @throws[ScriptException]
  def eval(reader: Reader, context: ScriptContext): Object = eval(stringFromReader(reader), context)

  private val buffer = new Array[Char](8192)

  def stringFromReader(in: Reader) = {
    val out = new StringWriter
    var n = in.read(buffer)
    while (n > -1) {
      out.write(buffer, 0, n)
      n = in.read(buffer)
    }
    in.close
    out.toString
  }
}

object ScriptEngine {
  import java.util.Arrays
  import scala.util.Properties

  class Factory extends ScriptEngineFactory {
    def getEngineName = "Scala REPL"
    def getEngineVersion = "3.0"
    def getExtensions = Arrays.asList("scala")
    def getLanguageName = "Scala"
    def getLanguageVersion = Properties.versionString
    def getMimeTypes = Arrays.asList("application/x-scala")
    def getNames = Arrays.asList("scala")

    def getMethodCallSyntax(obj: String, m: String, args: String*) = s"$obj.$m(${args.mkString(", ")})"

    def getOutputStatement(toDisplay: String) = s"""print("$toDisplay")"""

    def getParameter(key: String): Object = key match {
      case JScriptEngine.ENGINE           => getEngineName
      case JScriptEngine.ENGINE_VERSION   => getEngineVersion
      case JScriptEngine.LANGUAGE         => getLanguageName
      case JScriptEngine.LANGUAGE_VERSION => getLanguageVersion
      case JScriptEngine.NAME             => getNames.get(0)
      case _ => null
    }

    def getProgram(statements: String*) = statements.mkString("; ")

    def getScriptEngine: JScriptEngine = new ScriptEngine
  }
}
