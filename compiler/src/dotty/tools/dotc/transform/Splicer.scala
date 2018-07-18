package dotty.tools.dotc
package transform

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.Method

import dotty.tools.dotc.ast.tpd
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
  def splice(tree: Tree, call: Tree, bindings: List[Tree], pos: Position, classLoader: ClassLoader)(implicit ctx: Context): Tree = tree match {
    case Quoted(quotedTree) => quotedTree
    case _ =>
      val liftedArgs = getLiftedArgs(call, bindings)
      val interpreter = new Interpreter(pos, classLoader)
      val interpreted = interpreter.interpretCallToSymbol[Seq[Any] => Object](call.symbol)
      val tctx = new TastyImpl(ctx)
      evaluateMacro(pos) {
        // Some parts of the macro are evaluated during the unpickling performed in quotedExprToTree
        val evaluated = interpreted.map(lambda => lambda(tctx :: liftedArgs).asInstanceOf[scala.quoted.Expr[Nothing]])
        evaluated.fold(tree)(PickledQuotes.quotedExprToTree)
      }
  }

  /** Given the inline code and bindings, compute the lifted arguments that will be used to execute the macro
   *  - Type parameters are lifted to quoted.Types.TreeType
   *  - Inline parameters are listed as their value
   *  - Other parameters are lifted to quoted.Types.TreeExpr (may reference a binding)
   */
  private def getLiftedArgs(call: Tree, bindings: List[Tree])(implicit ctx: Context): List[Any] = {
    val bindMap = bindings.collect {
      case vdef: ValDef => (vdef.rhs, ref(vdef.symbol).withPos(vdef.rhs.pos))
    }.toMap
    def allArgs(call: Tree, acc: List[List[Tree]]): List[List[Tree]] = call match {
      case call: Apply => allArgs(call.fun, call.args :: acc)
      case call: TypeApply => allArgs(call.fun, call.args :: acc)
      case _ => acc
    }
    def liftArgs(tpe: Type, args: List[List[Tree]]): List[Any] = tpe match {
      case tp: MethodType =>
        val args1 = args.head.zip(tp.paramInfos).map {
          case (arg: Literal, tp) if tp.hasAnnotation(defn.TransparentParamAnnot) => arg.const.value
          case (arg, tp) =>
            assert(!tp.hasAnnotation(defn.TransparentParamAnnot))
            // Replace argument by its binding
            val arg1 = bindMap.getOrElse(arg, arg)
            new scala.quoted.Exprs.TastyTreeExpr(arg1)
        }
        args1 ::: liftArgs(tp.resType, args.tail)
      case tp: PolyType =>
        val args1 = args.head.map(tp => new scala.quoted.Types.TreeType(tp))
        args1 ::: liftArgs(tp.resType, args.tail)
      case _ => Nil
    }

    liftArgs(call.symbol.info, allArgs(call, Nil))
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
    def interpretCallToSymbol[T](sym: Symbol)(implicit ct: ClassTag[T]): Option[T] = {
      try {
        val (clazz, instance) = loadModule(sym.owner)
        val paramClasses = paramsSig(sym)
        val interpretedArgs = paramClasses.map(defaultValue)
        val method = getMethod(clazz, sym.name, paramClasses)
        stopIfRuntimeException(method.invoke(instance, interpretedArgs: _*)) match {
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
          val msg = s"Could not find transparent macro method ${clazz.getCanonicalName}.$name with parameters $paramClasses$extraMsg"
          throw new StopInterpretation(msg, pos)
      }
    }

    private def extraMsg = ". The most common reason for that is that you cannot use transparent macro implementations in the same compilation run that defines them"

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

    /** Get the default value for the given class */
    private def defaultValue(clazz: Class[_]): Object = {
      if (clazz == classOf[Boolean]) false.asInstanceOf[Object]
      else if (clazz == classOf[Byte]) 0.toByte.asInstanceOf[Object]
      else if (clazz == classOf[Char]) 0.toChar.asInstanceOf[Object]
      else if (clazz == classOf[Short]) 0.asInstanceOf[Object]
      else if (clazz == classOf[Int]) 0.asInstanceOf[Object]
      else if (clazz == classOf[Long]) 0L.asInstanceOf[Object]
      else if (clazz == classOf[Float]) 0f.asInstanceOf[Object]
      else if (clazz == classOf[Double]) 0d.asInstanceOf[Object]
      else null
    }

    /** Exception that stops interpretation if some issue is found */
    private class StopInterpretation(val msg: String, val pos: Position) extends Exception

  }

}
