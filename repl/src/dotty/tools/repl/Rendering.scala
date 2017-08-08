package dotty.tools
package repl

import scala.util.control.NonFatal

import dotc.core.Types._
import dotc.core.Contexts.Context
import dotc.core.Denotations.Denotation
import dotc.core.Flags
import dotc.core.Symbols.Symbol

object Rendering {
  /** Load the value of the symbol using reflection
   *
   *  Calling this method evaluates the expression using reflection
   */
  private[this] def valueOf(sym: Symbol, classLoader: ClassLoader)(implicit ctx: Context): Option[String] = {
    val defn = ctx.definitions
    val objectName = sym.owner.fullName.encode.toString.dropRight(1) // gotta drop the '$'
    val resObj: Class[_] = Class.forName(objectName, true, classLoader)

    val res =
      resObj
      .getDeclaredMethods.find(_.getName == sym.name.toString + "Show").get
      .invoke(null).toString

    val shellPrefix = "$none$.ReplSession$"
    if (!sym.is(Flags.Method) && sym.info == defn.UnitType)
      None
    else if (res.startsWith(shellPrefix))
      Some(res.drop(shellPrefix.length).dropWhile(c => c.isDigit || c == '$'))
    else
      Some(res)
  }

  /** Render method definition result */
  def renderMethod(d: Denotation)(implicit ctx: Context): String =
    d.symbol.showUser

  /** Render value definition result */
  def renderVal(d: Denotation, classLoader: ClassLoader)(implicit ctx: Context): String = {
    val dcl = d.symbol.showUser
    val resultValue =
      if (d.symbol.is(Flags.Lazy)) Some("<lazy>")
      else valueOf(d.symbol, classLoader)

    resultValue
      .map(value => s"$dcl = $value")
      .getOrElse("")
  }
}
