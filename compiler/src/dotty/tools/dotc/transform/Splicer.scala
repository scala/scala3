package dotty.tools.dotc
package transform

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.Method

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags.Package
import dotty.tools.dotc.core.NameKinds.FlatName
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.str.MODULE_INSTANCE_FIELD
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.tastyreflect.TastyImpl

import scala.util.control.NonFatal
import dotty.tools.dotc.util.Positions.Position

import scala.reflect.ClassTag

/** Utility class to splice quoted expressions */
object Splicer {
  import tpd._

  /** Splice the Tree for a Quoted expression. `~'(xyz)` becomes `xyz`
   *  and for `~xyz` the tree of `xyz` is interpreted for which the
   *  resulting expression is returned as a `Tree`
   *
   *  See: `ReifyQuotes`
   */
  def splice(tree: Tree, pos: Position, classLoader: ClassLoader)(implicit ctx: Context): Tree = tree match {
    case Quoted(quotedTree) => quotedTree
    case _ =>
      val interpreter = new Interpreter(pos, classLoader)
      evaluateMacro(pos) {
        // Some parts of the macro are evaluated during the unpickling performed in quotedExprToTree
        val interpreted = interpreter.interpretCall[scala.quoted.Expr[Any]](tree)
        interpreted.fold(tree)(x => PickledQuotes.quotedExprToTree(x))
      }
  }

  /* Evaluate the code in the macro and handle exceptions durring evaluation */
  private def evaluateMacro(pos: Position)(code: => Tree)(implicit ctx: Context): Tree = {
    try code
    catch {
      case ex: scala.quoted.QuoteError =>
        ctx.error(ex.getMessage, pos)
        EmptyTree
      case NonFatal(ex) =>
        val msg =
          s"""Failed to evaluate inlined quote.
             |  Caused by ${ex.getClass}: ${if (ex.getMessage == null) "" else ex.getMessage}
             |    ${ex.getStackTrace.takeWhile(_.getClassName != "dotty.tools.dotc.transform.Splicer$").init.mkString("\n    ")}
           """.stripMargin
        ctx.error(msg, pos)
        EmptyTree
    }
  }

  /** Tree interpreter that can interpret calls to static methods with it's default arguments
   *
   *  The interpreter assumes that all calls in the trees are to code that was
   *  previously compiled and is present in the classpath of the current context.
   */
  private class Interpreter(pos: Position, classLoader: ClassLoader)(implicit ctx: Context) {

    /** Returns the interpreted result of interpreting the code a call to the symbol with default arguments.
     *  Return Some of the result or None if some error happen during the interpretation.
     */
    def interpretCall[T](tree: Tree)(implicit ct: ClassTag[T]): Option[T] = {
      try {
        interpret(tree) match {
          case obj: T => Some(obj)
          case obj =>
            // TODO upgrade to a full type tag check or something similar
            ctx.error(s"Interpreted tree returned a result of an unexpected type. Expected ${ct.runtimeClass} but was ${obj.getClass}", pos)
            None
        }
      } catch {
        case ex: StopInterpretation =>
          ctx.error(ex.msg, ex.pos)
          None
      }
    }

    private def interpret(tree: Tree): Object = tree match {
        case Apply(TypeApply(fn, _), quoted :: Nil) if fn.symbol == defn.QuotedExpr_apply =>
          new scala.quoted.Exprs.TastyTreeExpr(quoted)
        case TypeApply(fn, quoted :: Nil) if fn.symbol == defn.QuotedType_apply =>
          new scala.quoted.Types.TreeType(quoted)

        case _ if tree.symbol == defn.TastyTopLevelSplice_tastyContext =>
          new TastyImpl(ctx)

        case Apply(fn @ Ident(name), args) if fn.symbol.isStatic =>
          val (clazz, instance) = loadModule(fn.symbol.owner)
          val method = getMethod(clazz, name, paramsSig(fn.symbol))
          val interpretedArgs = args.map(arg => interpret(arg))
          stopIfRuntimeException(method.invoke(instance, interpretedArgs: _*))
        case Apply(TypeApply(fn @ Ident(name), _), args) if fn.symbol.isStatic =>
          val (clazz, instance) = loadModule(fn.symbol.owner)
          val method = getMethod(clazz, name, paramsSig(fn.symbol))
          val interpretedArgs = args.map(arg => interpret(arg))
          stopIfRuntimeException(method.invoke(instance, interpretedArgs: _*))

        case Literal(Constant(value)) => value.asInstanceOf[Object]
    }

    private def loadModule(sym: Symbol): (Class[_], Object) = {
      if (sym.owner.is(Package)) {
        // is top level object
        val moduleClass = loadClass(sym.fullName)
        val moduleInstance = moduleClass.getField(MODULE_INSTANCE_FIELD).get(null)
        (moduleClass, moduleInstance)
      } else {
        // nested object in an object
        val clazz = loadClass(sym.fullNameSeparated(FlatName))
        (clazz, clazz.newInstance().asInstanceOf[Object])
      }
    }

    private def loadClass(name: Name): Class[_] = {
      try classLoader.loadClass(name.toString)
      catch {
        case _: ClassNotFoundException =>
          val msg = s"Could not find macro class $name in classpath$extraMsg"
          throw new StopInterpretation(msg, pos)
      }
    }

    private def getMethod(clazz: Class[_], name: Name, paramClasses: List[Class[_]]): Method = {
      try clazz.getMethod(name.toString, paramClasses: _*)
      catch {
        case _: NoSuchMethodException =>
          val msg = s"Could not find inline macro method ${clazz.getCanonicalName}.$name with parameters $paramClasses$extraMsg"
          throw new StopInterpretation(msg, pos)
      }
    }

    private def extraMsg = ". The most common reason for that is that you cannot use inline macro implementations in the same compilation run that defines them"

    private def stopIfRuntimeException[T](thunk: => T): T = {
      try thunk
      catch {
        case ex: RuntimeException =>
          val sw = new StringWriter()
          sw.write("A runtime exception occurred while executing macro expansion\n")
          sw.write(ex.getMessage)
          sw.write("\n")
          ex.printStackTrace(new PrintWriter(sw))
          sw.write("\n")
          throw new StopInterpretation(sw.toString, pos)
      }
    }

    /** List of classes of the parameters of the signature of `sym` */
    private def paramsSig(sym: Symbol): List[Class[_]] = {
      TypeErasure.erasure(sym.info) match {
        case meth: MethodType =>
          meth.paramInfos.map { param =>
            def arrayDepth(tpe: Type, depth: Int): (Type, Int) = tpe match {
              case JavaArrayType(elemType) => arrayDepth(elemType, depth + 1)
              case _ => (tpe, depth)
            }
            def javaArraySig(tpe: Type): String = {
              val (elemType, depth) = arrayDepth(tpe, 0)
              val sym = elemType.classSymbol
              val suffix =
                if (sym == defn.BooleanClass) "Z"
                else if (sym == defn.ByteClass) "B"
                else if (sym == defn.ShortClass) "S"
                else if (sym == defn.IntClass) "I"
                else if (sym == defn.LongClass) "J"
                else if (sym == defn.FloatClass) "F"
                else if (sym == defn.DoubleClass) "D"
                else if (sym == defn.CharClass) "C"
                else "L" + javaSig(elemType) + ";"
              ("[" * depth) + suffix
            }
            def javaSig(tpe: Type): String = tpe match {
              case tpe: JavaArrayType => javaArraySig(tpe)
              case _ =>
                // Take the flatten name of the class and the full package name
                val pack = tpe.classSymbol.topLevelClass.owner
                val packageName = if (pack == defn.EmptyPackageClass) "" else pack.fullName + "."
                packageName + tpe.classSymbol.fullNameSeparated(FlatName).toString
            }

            val sym = param.classSymbol
            if (sym == defn.BooleanClass) classOf[Boolean]
            else if (sym == defn.ByteClass) classOf[Byte]
            else if (sym == defn.CharClass) classOf[Char]
            else if (sym == defn.ShortClass) classOf[Short]
            else if (sym == defn.IntClass) classOf[Int]
            else if (sym == defn.LongClass) classOf[Long]
            else if (sym == defn.FloatClass) classOf[Float]
            else if (sym == defn.DoubleClass) classOf[Double]
            else java.lang.Class.forName(javaSig(param), false, classLoader)
          }
        case _ => Nil
      }
    }

    /** Exception that stops interpretation if some issue is found */
    private class StopInterpretation(val msg: String, val pos: Position) extends Exception

  }

}
