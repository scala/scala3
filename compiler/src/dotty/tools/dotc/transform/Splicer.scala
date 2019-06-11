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
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.quoted._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.{NameKinds, TypeErasure}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.tastyreflect.ReflectionImpl

import scala.util.control.NonFatal
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.repl.AbstractFileClassLoader

import scala.reflect.ClassTag

/** Utility class to splice quoted expressions */
object Splicer {
  import tpd._

  /** Splice the Tree for a Quoted expression. `${'{xyz}}` becomes `xyz`
   *  and for `$xyz` the tree of `xyz` is interpreted for which the
   *  resulting expression is returned as a `Tree`
   *
   *  See: `Staging`
   */
  def splice(tree: Tree, pos: SourcePosition, classLoader: ClassLoader)(implicit ctx: Context): Tree = tree match {
    case Quoted(quotedTree) => quotedTree
    case _ =>
      val interpreter = new Interpreter(pos, classLoader)
      try {
        // Some parts of the macro are evaluated during the unpickling performed in quotedExprToTree
        val interpretedExpr = interpreter.interpret[scala.quoted.Expr[Any]](tree)
        interpretedExpr.fold(tree)(x => PickledQuotes.quotedExprToTree(x))
      }
      catch {
        case ex: scala.quoted.QuoteError =>
          val pos1 = ex.from match {
            case None => pos
            case Some(expr) =>
              val reflect: scala.tasty.Reflection = ReflectionImpl(ctx)
              import reflect._
              expr.unseal.underlyingArgument.pos.asInstanceOf[SourcePosition]
          }
          ctx.error(ex.getMessage, pos1)
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

  /** Check that the Tree can be spliced. `${'{xyz}}` becomes `xyz`
    *  and for `$xyz` the tree of `xyz` is interpreted for which the
    *  resulting expression is returned as a `Tree`
    *
    *  See: `Staging`
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
      new scala.internal.quoted.TastyTreeExpr(Inlined(EmptyTree, Nil, tree).withSpan(tree.span))

    protected def interpretTypeQuote(tree: Tree)(implicit env: Env): Object =
      new scala.internal.quoted.TreeType(tree)

    protected def interpretLiteral(value: Any)(implicit env: Env): Object =
      value.asInstanceOf[Object]

    protected def interpretVarargs(args: List[Object])(implicit env: Env): Object =
      args.toSeq

    protected def interpretTastyContext()(implicit env: Env): Object = ReflectionImpl(ctx, pos)

    protected def interpretStaticMethodCall(moduleClass: Symbol, fn: Symbol, args: => List[Object])(implicit env: Env): Object = {
      val (inst, clazz) =
        if (moduleClass.name.startsWith(str.REPL_SESSION_LINE)) {
          (null, loadReplLineClass(moduleClass))
        } else {
          val inst = loadModule(moduleClass)
          (inst, inst.getClass)
        }

      def getDirectName(tp: Type, name: TermName): TermName = tp.widenDealias match {
        case tp: AppliedType if defn.isImplicitFunctionType(tp) =>
          getDirectName(tp.args.last, NameKinds.DirectMethodName(name))
        case _ => name
      }

      val name = getDirectName(fn.info.finalResultType, fn.name.asTermName)
      val method = getMethod(clazz, name, paramsSig(fn))
      stopIfRuntimeException(method.invoke(inst, args: _*))
    }

    protected def interpretModuleAccess(fn: Symbol)(implicit env: Env): Object =
      loadModule(fn.moduleClass)

    protected def interpretNew(fn: Symbol, args: => List[Result])(implicit env: Env): Object = {
      val clazz = loadClass(fn.owner.fullName)
      val constr = clazz.getConstructor(paramsSig(fn): _*)
      constr.newInstance(args: _*).asInstanceOf[Object]
    }

    protected def unexpectedTree(tree: Tree)(implicit env: Env): Object =
      throw new StopInterpretation("Unexpected tree could not be interpreted: " + tree, tree.sourcePos)

    private def loadModule(sym: Symbol): Object = {
      if (sym.owner.is(Package)) {
        // is top level object
        val moduleClass = loadClass(sym.fullName)
        moduleClass.getField(str.MODULE_INSTANCE_FIELD).get(null)
      } else {
        // nested object in an object
        val clazz = loadClass(sym.fullNameSeparated(FlatName))
        clazz.getConstructor().newInstance().asInstanceOf[Object]
      }
    }

    private def loadReplLineClass(moduleClass: Symbol)(implicit env: Env): Class[_] = {
      val lineClassloader = new AbstractFileClassLoader(ctx.settings.outputDir.value, classLoader)
      lineClassloader.loadClass(moduleClass.name.firstPart.toString)
    }

    private def loadClass(name: Name): Class[_] = {
      try classLoader.loadClass(name.toString)
      catch {
        case _: ClassNotFoundException =>
          val msg = s"Could not find class $name in classpath$extraMsg"
          throw new StopInterpretation(msg, pos)
      }
    }

    private def getMethod(clazz: Class[_], name: Name, paramClasses: List[Class[_]]): Method = {
      try clazz.getMethod(name.toString, paramClasses: _*)
      catch {
        case _: NoSuchMethodException =>
          val msg = em"Could not find method ${clazz.getCanonicalName}.$name with parameters ($paramClasses%, %)$extraMsg"
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
      def paramClass(param: Type): Class[_] = {
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
      def getExtraParams(tp: Type): List[Type] = tp.widenDealias match {
        case tp: AppliedType if defn.isImplicitFunctionType(tp) =>
          // Call implicit function type direct method
          tp.args.init.map(arg => TypeErasure.erasure(arg)) ::: getExtraParams(tp.args.last)
        case _ => Nil
      }
      val extraParams = getExtraParams(sym.info.finalResultType)
      val allParams = TypeErasure.erasure(sym.info) match {
        case meth: MethodType => meth.paramInfos ::: extraParams
        case _ => extraParams
      }
      allParams.map(paramClass)
    }

    /** Exception that stops interpretation if some issue is found */
    private class StopInterpretation(val msg: String, val pos: SourcePosition) extends Exception

  }

  /** Tree interpreter that tests if tree can be interpreted */
  private class CanBeInterpreted(implicit ctx: Context) extends AbstractInterpreter {

    type Result = Boolean

    def apply(tree: Tree): Boolean = interpretTree(tree)(Map.empty)

    protected def interpretQuote(tree: tpd.Tree)(implicit env: Env): Boolean = true
    protected def interpretTypeQuote(tree: tpd.Tree)(implicit env: Env): Boolean = true
    protected def interpretLiteral(value: Any)(implicit env: Env): Boolean = true
    protected def interpretVarargs(args: List[Boolean])(implicit env: Env): Boolean = args.forall(identity)
    protected def interpretTastyContext()(implicit env: Env): Boolean = true
    protected def interpretQuoteContext()(implicit env: Env): Boolean = true
    protected def interpretStaticMethodCall(module: Symbol, fn: Symbol, args: => List[Boolean])(implicit env: Env): Boolean = args.forall(identity)
    protected def interpretModuleAccess(fn: Symbol)(implicit env: Env): Boolean = true
    protected def interpretNew(fn: Symbol, args: => List[Boolean])(implicit env: Env): Boolean = args.forall(identity)

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
    protected def interpretVarargs(args: List[Result])(implicit env: Env): Result
    protected def interpretTastyContext()(implicit env: Env): Result
    protected def interpretStaticMethodCall(module: Symbol, fn: Symbol, args: => List[Result])(implicit env: Env): Result
    protected def interpretModuleAccess(fn: Symbol)(implicit env: Env): Result
    protected def interpretNew(fn: Symbol, args: => List[Result])(implicit env: Env): Result
    protected def unexpectedTree(tree: Tree)(implicit env: Env): Result

    protected final def interpretTree(tree: Tree)(implicit env: Env): Result = tree match {
      case Apply(TypeApply(fn, _), quoted :: Nil) if fn.symbol == defn.InternalQuoted_exprQuote =>
        val quoted1 = quoted match {
          case quoted: Ident if quoted.symbol.is(InlineByNameProxy) =>
            // inline proxy for by-name parameter
            quoted.symbol.defTree.asInstanceOf[DefDef].rhs
          case Inlined(EmptyTree, _, quoted) => quoted
          case _ => quoted
        }
        interpretQuote(quoted1)

      case TypeApply(fn, quoted :: Nil) if fn.symbol == defn.InternalQuoted_typeQuote =>
        interpretTypeQuote(quoted)

      case Literal(Constant(value)) =>
        interpretLiteral(value)

      case _ if tree.symbol == defn.TastyReflection_macroContext =>
        interpretTastyContext()

      case Call(fn, args) =>
        if (fn.symbol.isConstructor && fn.symbol.owner.owner.is(Package)) {
          interpretNew(fn.symbol, args.map(interpretTree))
        } else if (fn.symbol.is(Module)) {
          interpretModuleAccess(fn.symbol)
        } else if (fn.symbol.isStatic) {
          val module = fn.symbol.owner
          interpretStaticMethodCall(module, fn.symbol, args.map(arg => interpretTree(arg)))
        } else if (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic) {
          val module = fn.qualifier.symbol.moduleClass
          interpretStaticMethodCall(module, fn.symbol, args.map(arg => interpretTree(arg)))
        } else if (env.contains(fn.name)) {
          env(fn.name)
        } else if (tree.symbol.is(InlineProxy)) {
          interpretTree(tree.symbol.defTree.asInstanceOf[ValOrDefDef].rhs)
        } else {
          unexpectedTree(tree)
        }

      // Interpret `foo(j = x, i = y)` which it is expanded to
      // `val j$1 = x; val i$1 = y; foo(i = y, j = x)`
      case Block(stats, expr) =>
        var unexpected: Option[Result] = None
        val newEnv = stats.foldLeft(env)((accEnv, stat) => stat match {
          case stat: ValDef if stat.symbol.is(Synthetic) =>
            accEnv.updated(stat.name, interpretTree(stat.rhs)(accEnv))
          case stat =>
            if (unexpected.isEmpty)
              unexpected = Some(unexpectedTree(stat))
            accEnv
        })
        unexpected.getOrElse(interpretTree(expr)(newEnv))
      case NamedArg(_, arg) => interpretTree(arg)

      case Inlined(_, Nil, expansion) => interpretTree(expansion)

      case Typed(expr, _) =>
        interpretTree(expr)

      case SeqLiteral(elems, _) =>
        interpretVarargs(elems.map(e => interpretTree(e)))

      case _ =>
        unexpectedTree(tree)
    }

    object Call {
      def unapply(arg: Tree): Option[(RefTree, List[Tree])] = arg match {
        case Select(Call(fn, args), nme.apply) if defn.isImplicitFunctionType(fn.tpe.widenDealias.finalResultType) =>
          Some((fn, args))
        case fn: RefTree => Some((fn, Nil))
        case Apply(Call(fn, args1), args2) => Some((fn, args1 ::: args2)) // TODO improve performance
        case TypeApply(Call(fn, args), _) => Some((fn, args))
        case _ => None
      }
    }
  }

}
