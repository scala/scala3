package dotty.tools.dotc
package interpreter

import java.io.{PrintWriter, StringWriter}

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.quoted.Quoted
import dotty.tools.dotc.util.Positions.Position

import scala.reflect.ClassTag
import java.net.URLClassLoader
import java.lang.reflect.Constructor
import java.lang.reflect.Method

/** Tree interpreter that can interpret
 *   * Literal constants
 *   * Calls to static methods
 *   * New objects with explicit `new` keyword
 *   * Quoted code
 *
 *  The interpreter assumes that all calls in the trees are to code that was
 *  previously compiled and is present in the classpath of the current context.
 */
class Interpreter(implicit ctx: Context) {
  import tpd._

  type Env = Map[Symbol, Object]

  private[this] val classLoader = {
    val urls = ctx.settings.classpath.value.split(':').map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
    new URLClassLoader(urls, getClass.getClassLoader)
  }

  /** Returns the interpreted result of interpreting the code represented by the tree.
   *  Return Some of the result or None if some error happen during the interpretation.
   */
  def interpretTree[T](tree: Tree)(implicit ct: ClassTag[T]): Option[T] = {
    try {
      interpretTreeImpl(tree, Map.empty) match {
        case obj: T => Some(obj)
        case obj =>
          // TODO upgrade to a full type tag check or something similar
          ctx.error(s"Interpreted tree returned a result of an unexpected type. Expected ${ct.runtimeClass} but was ${obj.getClass}", tree.pos)
          None
      }
    } catch {
      case ex: StopInterpretation =>
        ctx.error(ex.msg, ex.pos)
        None
    }
  }

  /** Returns the interpreted result of interpreting the code represented by the tree.
   *  Returns the result of the interpreted tree.
   *
   *  If some error is encountered while interpreting a ctx.error is emitted and a StopInterpretation is thrown.
   */
  private def interpretTreeImpl(tree: Tree, env: Env): Object = {
    ctx.debuglog(
      s"""Interpreting:
        |${tree.show}
        |$env
      """.stripMargin)

    implicit val pos: Position = tree.pos

    tree match {
      case Quoted(quotedTree) => RawQuoted(quotedTree)

      case Literal(Constant(c)) => c.asInstanceOf[Object]

      case Apply(fn, args) if fn.symbol.isConstructor =>
        val clazz = loadClass(fn.symbol.owner.symbol.fullName)
        val paramClasses = paramsSig(fn.symbol)
        val interpretedArgs = args.map(arg => interpretTreeImpl(arg, env))
        val constructor = getConstructor(clazz, paramClasses)
        interpreted(constructor.newInstance(interpretedArgs: _*))

      case _: RefTree | _: Apply if tree.symbol.isStatic =>
        val clazz = loadClass(tree.symbol.owner.companionModule.fullName)
        val paramClasses = paramsSig(tree.symbol)
        val interpretedArgs = Array.newBuilder[Object]
        def interpretArgs(tree: Tree): Unit = tree match {
          case Apply(fn, args) =>
            interpretArgs(fn)
            args.foreach(arg => interpretedArgs += interpretTreeImpl(arg, env))
          case _ =>
        }
        interpretArgs(tree)

        val method = getMethod(clazz, tree.symbol.name, paramClasses)
        interpreted(method.invoke(null, interpretedArgs.result(): _*))

      case tree: Ident if env.contains(tree.symbol) =>
        env(tree.symbol)

      case Block(stats, expr) =>
        val env2 = stats.foldLeft(env)((acc, x) => interpretStat(x, acc))
        interpretTreeImpl(expr, env2)

      case tree: NamedArg =>
        interpretTreeImpl(tree.arg, env)

      case Inlined(_, bindings, expansion) =>
        val env2 = bindings.foldLeft(env)((acc, x) => interpretStat(x, acc))
        interpretTreeImpl(expansion, env2)

      case _ =>
        // TODO Add more precise descriptions of why it could not be interpreted.
        // This should be done after the full interpreter is implemented.
        throw new StopInterpretation(s"Could not interpret ${tree.show}\n${tree}", tree.pos)
    }
  }

  /** Interprets the statement and returns the updated environment */
  private def interpretStat(stat: Tree, env: Env): Env = stat match {
    case tree: ValDef =>
      val obj = interpretTreeImpl(tree.rhs, env)
      env.updated(tree.symbol, obj)

    case _ =>
      interpretTreeImpl(stat, env)
      env
  }

  private def loadClass(name: Name)(implicit pos: Position): Class[_] = {
    try classLoader.loadClass(name.toString)
    catch {
      case _: ClassNotFoundException =>
        val msg = s"Could not find interpreted class $name in classpath"
        throw new StopInterpretation(msg, pos)
    }
  }

  private def getMethod(clazz: Class[_], name: Name, paramClasses: List[Class[_]])(implicit pos: Position): Method = {
    try clazz.getMethod(name.toString, paramClasses: _*)
    catch {
      case _: NoSuchMethodException =>
        val msg = s"Could not find interpreted method ${clazz.getCanonicalName}.$name with parameters $paramClasses"
        throw new StopInterpretation(msg, pos)
    }
  }

  private def getConstructor(clazz: Class[_], paramClasses: List[Class[_]])(implicit pos: Position): Constructor[Object] = {
    try clazz.getConstructor(paramClasses: _*).asInstanceOf[Constructor[Object]]
    catch {
      case _: NoSuchMethodException =>
        val msg = s"Could not find interpreted constructor of ${clazz.getCanonicalName} with parameters $paramClasses"
        throw new StopInterpretation(msg, pos)
    }
  }

  private def interpreted[T](thunk: => T)(implicit pos: Position): T = {
    try thunk
    catch {
      case ex: RuntimeException =>
        val sw = new StringWriter()
        sw.write("A runtime exception occurred while interpreting\n")
        sw.write(ex.getMessage)
        sw.write("\n")
        ex.printStackTrace(new PrintWriter(sw))
        sw.write("\n")
        throw new StopInterpretation(sw.toString, pos)
    }
  }

  /** List of classes of the parameters of the signature of `sym` */
  private def paramsSig(sym: Symbol): List[Class[_]] = {
    sym.signature.paramsSig.map { param =>
      defn.valueTypeNameToJavaType(param) match {
        case Some(clazz) => clazz
        case None => classLoader.loadClass(param.toString)
      }
    }
  }

  /** Exception that stops interpretation if some issue is found */
  private class StopInterpretation(val msg: String, val pos: Position) extends Exception

}
