package dotty.tools.dotc
package transform

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.{InvocationTargetException, Method}

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
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
import dotty.tools.dotc.util.SourcePosition

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
  def splice(tree: Tree, pos: SourcePosition, classLoader: ClassLoader)(implicit ctx: Context): Tree = tree match {
    case Quoted(quotedTree) => quotedTree
    case _ =>
      val interpreter = new Interpreter(pos, classLoader)
      try {
        // Some parts of the macro are evaluated during the unpickling performed in quotedExprToTree
        val interpreted = interpreter.interpret[scala.quoted.Expr[Any]](tree)
        interpreted.fold(tree)(x => PickledQuotes.quotedExprToTree(x))
      }
      catch {
        case ex: scala.quoted.QuoteError =>
          ctx.error(ex.getMessage, pos)
          EmptyTree
        case NonFatal(ex) =>
          val msg =
            s"""Failed to evaluate macro.
               |  Caused by ${ex.getClass}: ${if (ex.getMessage == null) "" else ex.getMessage}
               |    ${ex.getStackTrace.takeWhile(_.getClassName != "dotty.tools.dotc.transform.Splicer$").init.mkString("\n    ")}
             """.stripMargin
          ctx.error(msg, pos)
          EmptyTree
      }
  }

  /** Check that the Tree can be spliced. `~'(xyz)` becomes `xyz`
    *  and for `~xyz` the tree of `xyz` is interpreted for which the
    *  resulting expression is returned as a `Tree`
    *
    *  See: `ReifyQuotes`
    */
  def canBeSpliced(tree: Tree)(implicit ctx: Context): Boolean = tree match {
    case Quoted(_) => true
    case _ => (new CanBeInterpreted).apply(tree)
  }

  /** Tree interpreter that evaluates the tree */
  private class Interpreter(pos: SourcePosition, classLoader: ClassLoader)(implicit ctx: Context) extends AbstractInterpreter {

    type Result = Object

    /** Returns the interpreted result of interpreting the code a call to the symbol with default arguments.
     *  Return Some of the result or None if some error happen during the interpretation.
     */
    def interpret[T](tree: Tree)(implicit ct: ClassTag[T]): Option[T] = {
      try {
        interpretTree(tree)(Map.empty) match {
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

    protected def interpretQuote(tree: Tree)(implicit env: Env): Object =
      new scala.quoted.Exprs.TastyTreeExpr(tree)

    protected def interpretTypeQuote(tree: Tree)(implicit env: Env): Object =
      new scala.quoted.Types.TreeType(tree)

    protected def interpretLiteral(value: Any)(implicit env: Env): Object =
      value.asInstanceOf[Object]

    protected def interpretTastyContext()(implicit env: Env): Object =
      new TastyImpl(ctx) {
        override def rootPosition: SourcePosition = pos
      }

    protected def interpretStaticMethodCall(fn: Tree, args: => List[Object])(implicit env: Env): Object = {
      val (clazz, instance) = loadModule(fn.symbol.owner)
      val method = getMethod(clazz, fn.symbol.name, paramsSig(fn.symbol))
      stopIfRuntimeException(method.invoke(instance, args: _*))
    }

    protected def unexpectedTree(tree: Tree)(implicit env: Env): Object =
      throw new StopInterpretation("Unexpected tree could not be interpreted: " + tree, tree.pos)

    private def loadModule(sym: Symbol): (Class[_], Object) = {
      if (sym.owner.is(Package)) {
        // is top level object
        val moduleClass = loadClass(sym.fullName)
        val moduleInstance = moduleClass.getField(MODULE_INSTANCE_FIELD).get(null)
        (moduleClass, moduleInstance)
      } else {
        // nested object in an object
        val clazz = loadClass(sym.fullNameSeparated(FlatName))
        (clazz, clazz.getConstructor().newInstance().asInstanceOf[Object])
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
          val msg = em"Could not find macro method ${clazz.getCanonicalName}.$name with parameters ($paramClasses%, %)$extraMsg"
          throw new StopInterpretation(msg, pos)
      }
    }

    private def extraMsg = ". The most common reason for that is that you apply macros in the compilation run that defines them"

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
        case ex: InvocationTargetException =>
          ex.getCause match {
            case cause: scala.quoted.QuoteError =>
              throw cause
            case _ =>
              val sw = new StringWriter()
              sw.write("An exception occurred while executing macro expansion\n")
              sw.write(ex.getTargetException.getMessage)
              sw.write("\n")
              ex.getTargetException.printStackTrace(new PrintWriter(sw))
              sw.write("\n")
              throw new StopInterpretation(sw.toString, pos)
          }

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
    private class StopInterpretation(val msg: String, val pos: SourcePosition) extends Exception

  }

  /** Tree interpreter that tests if tree can be interpreted */
  private class CanBeInterpreted(implicit ctx: Context) extends AbstractInterpreter {

    type Result = Boolean

    def apply(tree: Tree): Boolean = interpretTree(tree)(Map.empty)

    def interpretQuote(tree: tpd.Tree)(implicit env: Env): Boolean = true
    def interpretTypeQuote(tree: tpd.Tree)(implicit env: Env): Boolean = true
    def interpretLiteral(value: Any)(implicit env: Env): Boolean = true
    def interpretTastyContext()(implicit env: Env): Boolean = true
    def interpretStaticMethodCall(fn: tpd.Tree, args: => List[Boolean])(implicit env: Env): Boolean = args.forall(identity)

    def unexpectedTree(tree: tpd.Tree)(implicit env: Env): Boolean = {
      // Assuming that top-level splices can only be in inline methods
      // and splices are expanded at inline site, references to inline values
      // will be known literal constant trees.
      tree.symbol.is(Inline)
    }
  }

  /** Abstract Tree interpreter that can interpret calls to static methods with quoted or inline arguments */
  private abstract class AbstractInterpreter(implicit ctx: Context) {
    type Env = Map[Name, Result]
    type Result

    protected def interpretQuote(tree: Tree)(implicit env: Env): Result
    protected def interpretTypeQuote(tree: Tree)(implicit env: Env): Result
    protected def interpretLiteral(value: Any)(implicit env: Env): Result
    protected def interpretTastyContext()(implicit env: Env): Result
    protected def interpretStaticMethodCall(fn: Tree, args: => List[Result])(implicit env: Env): Result
    protected def unexpectedTree(tree: Tree)(implicit env: Env): Result

    protected final def interpretTree(tree: Tree)(implicit env: Env): Result = tree match {
      case Apply(TypeApply(fn, _), quoted :: Nil) if fn.symbol == defn.QuotedExpr_apply =>
        interpretQuote(quoted)

      case TypeApply(fn, quoted :: Nil) if fn.symbol == defn.QuotedType_apply =>
        interpretTypeQuote(quoted)

      case Literal(Constant(value)) =>
        interpretLiteral(value)

      case _ if tree.symbol == defn.TastyTasty_macroContext =>
        interpretTastyContext()

      case StaticMethodCall(fn, args) =>
        interpretStaticMethodCall(fn, args.map(arg => interpretTree(arg)))

      // Interpret `foo(j = x, i = y)` which it is expanded to
      // `val j$1 = x; val i$1 = y; foo(i = y, j = x)`
      case Block(stats, expr) =>
        val newEnv = stats.foldLeft(env)((accEnv, stat) => stat match {
          case stat: ValDef if stat.symbol.is(Synthetic) =>
            accEnv.updated(stat.name, interpretTree(stat.rhs)(accEnv))
          case stat => return unexpectedTree(stat)
        })
        interpretTree(expr)(newEnv)
      case NamedArg(_, arg) => interpretTree(arg)
      case Ident(name) if env.contains(name) => env(name)

      case Inlined(EmptyTree, Nil, expansion) => interpretTree(expansion)

      case _ => unexpectedTree(tree)
    }

    object StaticMethodCall {
      def unapply(arg: Tree): Option[(RefTree, List[Tree])] = arg match {
        case fn: RefTree if fn.symbol.isStatic => Some((fn, Nil))
        case Apply(StaticMethodCall(fn, args1), args2) => Some((fn, args1 ::: args2)) // TODO improve performance
        case TypeApply(StaticMethodCall(fn, args), _) => Some((fn, args))
        case _ => None
      }
    }
  }

}
