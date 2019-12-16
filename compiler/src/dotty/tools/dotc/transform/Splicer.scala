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
import dotty.tools.dotc.tastyreflect.{ReflectionImpl, TastyTreeExpr, TreeType}

import scala.util.control.NonFatal
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.repl.AbstractFileClassLoader

import scala.reflect.ClassTag

import dotty.tools.dotc.quoted.QuoteContext

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
        val interpretedExpr = interpreter.interpret[scala.quoted.QuoteContext => scala.quoted.Expr[Any]](tree)
        interpretedExpr.fold(tree)(macroClosure => PickledQuotes.quotedExprToTree(macroClosure(QuoteContext())))
      }
      catch {
        case ex: CompilationUnit.SuspendException =>
          throw ex
        case ex: StopInterpretation =>
          ctx.error(ex.msg, ex.pos)
          EmptyTree
        case NonFatal(ex) =>
          val msg =
            s"""Failed to evaluate macro.
               |  Caused by ${ex.getClass}: ${if (ex.getMessage == null) "" else ex.getMessage}
               |    ${ex.getStackTrace.takeWhile(_.getClassName != "dotty.tools.dotc.transform.Splicer$").drop(1).mkString("\n    ")}
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
  def checkValidMacroBody(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case Quoted(_) => // ok
    case _ =>
      type Env = Set[Symbol]

      def checkValidStat(tree: Tree)(given Env): Env = tree match {
        case tree: ValDef if tree.symbol.is(Synthetic) =>
          // Check val from `foo(j = x, i = y)` which it is expanded to
          // `val j$1 = x; val i$1 = y; foo(i = i$1, j = j$1)`
          checkIfValidArgument(tree.rhs)
          summon[Env] + tree.symbol
        case _ =>
          ctx.error("Macro should not have statements", tree.sourcePos)
          summon[Env]
      }

      def checkIfValidArgument(tree: Tree)(given Env): Unit = tree match {
        case Block(Nil, expr) => checkIfValidArgument(expr)
        case Typed(expr, _) => checkIfValidArgument(expr)

        case Apply(Select(Apply(fn, quoted :: Nil), nme.apply), _) if fn.symbol == defn.InternalQuoted_exprQuote =>
          // OK

        case TypeApply(fn, quoted :: Nil) if fn.symbol == defn.InternalQuoted_typeQuote =>
          // OK

        case Literal(Constant(value)) =>
          // OK

        case Call(fn, args)
            if (fn.symbol.isConstructor && fn.symbol.owner.owner.is(Package)) ||
               fn.symbol.is(Module) || fn.symbol.isStatic ||
               (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic) =>
          args.foreach(_.foreach(checkIfValidArgument))

        case NamedArg(_, arg) =>
          checkIfValidArgument(arg)

        case SeqLiteral(elems, _) =>
          elems.foreach(checkIfValidArgument)

        case tree: Ident if tree.symbol.is(Inline) || summon[Env].contains(tree.symbol) =>
          // OK

        case _ =>
          ctx.error(
            """Malformed macro parameter
              |
              |Parameters may be:
              | * Quoted parameters or fields
              | * References to inline parameters
              | * Literal values of primitive types
              |""".stripMargin, tree.sourcePos)
      }

      def checkIfValidStaticCall(tree: Tree)(given Env): Unit = tree match {
        case closureDef(ddef @ DefDef(_, Nil, (ev :: Nil) :: Nil, _, _)) if ddef.symbol.info.isContextualMethod =>
          checkIfValidStaticCall(ddef.rhs)(given summon[Env] + ev.symbol)

        case Block(stats, expr) =>
          val newEnv = stats.foldLeft(summon[Env])((env, stat) => checkValidStat(stat)(given env))
          checkIfValidStaticCall(expr)(given newEnv)

        case Typed(expr, _) =>
          checkIfValidStaticCall(expr)

        case Call(fn, args)
            if (fn.symbol.isConstructor && fn.symbol.owner.owner.is(Package)) ||
               fn.symbol.is(Module) || fn.symbol.isStatic ||
               (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic) =>
          if (fn.symbol.flags.is(Inline))
            ctx.error("Macro cannot be implemented with an `inline` method", fn.sourcePos)
          args.flatten.foreach(checkIfValidArgument)

        case _ =>
          ctx.error(
            """Malformed macro.
              |
              |Expected the splice ${...} to contain a single call to a static method.
              |""".stripMargin, tree.sourcePos)
      }

      checkIfValidStaticCall(tree)(given Set.empty)
  }

  /** Tree interpreter that evaluates the tree */
  private class Interpreter(pos: SourcePosition, classLoader: ClassLoader)(implicit ctx: Context) {

    type Env = Map[Symbol, Object]

    /** Returns the interpreted result of interpreting the code a call to the symbol with default arguments.
     *  Return Some of the result or None if some error happen during the interpretation.
     */
    def interpret[T](tree: Tree)(implicit ct: ClassTag[T]): Option[T] =
      interpretTree(tree)(Map.empty) match {
        case obj: T => Some(obj)
        case obj =>
          // TODO upgrade to a full type tag check or something similar
          ctx.error(s"Interpreted tree returned a result of an unexpected type. Expected ${ct.runtimeClass} but was ${obj.getClass}", pos)
          None
      }

    def interpretTree(tree: Tree)(implicit env: Env): Object = tree match {
      case Apply(Select(Apply(TypeApply(fn, _), quoted :: Nil), nme.apply), _) if fn.symbol == defn.InternalQuoted_exprQuote =>
        val quoted1 = quoted match {
          case quoted: Ident if quoted.symbol.isAllOf(InlineByNameProxy) =>
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

      // TODO disallow interpreted method calls as arguments
      case Call(fn, args) =>
        if (fn.symbol.isConstructor && fn.symbol.owner.owner.is(Package))
          interpretNew(fn.symbol, args.flatten.map(interpretTree))
        else if (fn.symbol.is(Module))
          interpretModuleAccess(fn.symbol)
        else if (fn.symbol.isStatic) {
          val staticMethodCall = interpretedStaticMethodCall(fn.symbol.owner, fn.symbol)
          staticMethodCall(args.flatten.map(interpretTree))
        }
        else if (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic)
          if (fn.name == nme.asInstanceOfPM)
            interpretModuleAccess(fn.qualifier.symbol)
          else {
            val staticMethodCall = interpretedStaticMethodCall(fn.qualifier.symbol.moduleClass, fn.symbol)
            staticMethodCall(args.flatten.map(interpretTree))
          }
        else if (env.contains(fn.symbol))
          env(fn.symbol)
        else if (tree.symbol.is(InlineProxy))
          interpretTree(tree.symbol.defTree.asInstanceOf[ValOrDefDef].rhs)
        else
          unexpectedTree(tree)

      case closureDef((ddef @ DefDef(_, _, (arg :: Nil) :: Nil, _, _))) =>
        (obj: AnyRef) => interpretTree(ddef.rhs)(given env.updated(arg.symbol, obj))

      // Interpret `foo(j = x, i = y)` which it is expanded to
      // `val j$1 = x; val i$1 = y; foo(i = i$1, j = j$1)`
      case Block(stats, expr) => interpretBlock(stats, expr)
      case NamedArg(_, arg) => interpretTree(arg)

      case Inlined(_, bindings, expansion) => interpretBlock(bindings, expansion)

      case Typed(expr, _) =>
        interpretTree(expr)

      case SeqLiteral(elems, _) =>
        interpretVarargs(elems.map(e => interpretTree(e)))

      case _ =>
        unexpectedTree(tree)
    }

    private def interpretBlock(stats: List[Tree], expr: Tree)(implicit env: Env) = {
      var unexpected: Option[Object] = None
      val newEnv = stats.foldLeft(env)((accEnv, stat) => stat match {
        case stat: ValDef =>
          accEnv.updated(stat.symbol, interpretTree(stat.rhs)(accEnv))
        case stat =>
          if (unexpected.isEmpty)
            unexpected = Some(unexpectedTree(stat))
          accEnv
      })
      unexpected.getOrElse(interpretTree(expr)(newEnv))
    }

    private def interpretQuote(tree: Tree)(implicit env: Env): Object =
      new TastyTreeExpr(Inlined(EmptyTree, Nil, tree).withSpan(tree.span), QuoteContext.scopeId)

    private def interpretTypeQuote(tree: Tree)(implicit env: Env): Object =
      new TreeType(tree, QuoteContext.scopeId)

    private def interpretLiteral(value: Any)(implicit env: Env): Object =
      value.asInstanceOf[Object]

    private def interpretVarargs(args: List[Object])(implicit env: Env): Object =
      args.toSeq

    private def interpretedStaticMethodCall(moduleClass: Symbol, fn: Symbol)(implicit env: Env): List[Object] => Object = {
      val (inst, clazz) =
        if (moduleClass.name.startsWith(str.REPL_SESSION_LINE))
          (null, loadReplLineClass(moduleClass))
        else {
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
      (args: List[Object]) => stopIfRuntimeException(method.invoke(inst, args: _*), method)
    }

    private def interpretModuleAccess(fn: Symbol)(implicit env: Env): Object =
      loadModule(fn.moduleClass)

    private def interpretNew(fn: Symbol, args: => List[Object])(implicit env: Env): Object = {
      val clazz = loadClass(fn.owner.fullName.toString)
      val constr = clazz.getConstructor(paramsSig(fn): _*)
      constr.newInstance(args: _*).asInstanceOf[Object]
    }

    private def unexpectedTree(tree: Tree)(implicit env: Env): Object =
      throw new StopInterpretation("Unexpected tree could not be interpreted: " + tree, tree.sourcePos)

    private def loadModule(sym: Symbol): Object =
      if (sym.owner.is(Package)) {
        // is top level object
        val moduleClass = loadClass(sym.fullName.toString)
        moduleClass.getField(str.MODULE_INSTANCE_FIELD).get(null)
      }
      else {
        // nested object in an object
        val className = {
          val pack = sym.topLevelClass.owner
          if (pack == defn.RootPackage || pack == defn.EmptyPackageClass) sym.flatName.toString
          else pack.showFullName + "." + sym.flatName
        }
        val clazz = loadClass(className)
        clazz.getConstructor().newInstance().asInstanceOf[Object]
      }

    private def loadReplLineClass(moduleClass: Symbol)(implicit env: Env): Class[?] = {
      val lineClassloader = new AbstractFileClassLoader(ctx.settings.outputDir.value, classLoader)
      lineClassloader.loadClass(moduleClass.name.firstPart.toString)
    }

    private def loadClass(name: String): Class[?] =
      try classLoader.loadClass(name)
      catch {
        case _: ClassNotFoundException =>
          val msg = s"Could not find class $name in classpath"
          throw new StopInterpretation(msg, pos)
      }

    private def getMethod(clazz: Class[?], name: Name, paramClasses: List[Class[?]]): Method =
      try clazz.getMethod(name.toString, paramClasses: _*)
      catch {
        case _: NoSuchMethodException =>
          val msg = em"Could not find method ${clazz.getCanonicalName}.$name with parameters ($paramClasses%, %)"
          throw new StopInterpretation(msg, pos)
      }

    private def stopIfRuntimeException[T](thunk: => T, method: Method): T =
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
          ex.getTargetException match {
            case MissingClassDefinedInCurrentRun(sym) =>
              if (ctx.settings.XprintSuspension.value)
                ctx.echo(i"suspension triggered by a dependency on $sym", pos)
              ctx.compilationUnit.suspend() // this throws a SuspendException
            case targetException =>
              val sw = new StringWriter()
              sw.write("Exception occurred while executing macro expansion.\n")
              if (!ctx.settings.Ydebug.value) {
                val end = targetException.getStackTrace.lastIndexWhere { x =>
                  x.getClassName == method.getDeclaringClass.getCanonicalName && x.getMethodName == method.getName
                }
                val shortStackTrace = targetException.getStackTrace.take(end + 1)
                targetException.setStackTrace(shortStackTrace)
              }
              targetException.printStackTrace(new PrintWriter(sw))
              sw.write("\n")
              throw new StopInterpretation(sw.toString, pos)
          }
      }

    private object MissingClassDefinedInCurrentRun {
      def unapply(targetException: NoClassDefFoundError)(given ctx: Context): Option[Symbol] = {
        val className = targetException.getMessage
        if (className eq null) None
        else {
          val sym = ctx.base.staticRef(className.toTypeName).symbol
          if (sym.isDefinedInCurrentRun) Some(sym) else None
        }
      }
    }

    /** List of classes of the parameters of the signature of `sym` */
    private def paramsSig(sym: Symbol): List[Class[?]] = {
      def paramClass(param: Type): Class[?] = {
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
            val packageName = if (pack == defn.EmptyPackageClass) "" else s"${pack.fullName}."
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
  }



  /** Exception that stops interpretation if some issue is found */
  private class StopInterpretation(val msg: String, val pos: SourcePosition) extends Exception

  object Call {
    /** Matches an expression that is either a field access or an application
     *  It retruns a TermRef containing field accessed or a method reference and the arguments passed to it.
     */
    def unapply(arg: Tree)(implicit ctx: Context): Option[(RefTree, List[List[Tree]])] =
      Call0.unapply(arg).map((fn, args) => (fn, args.reverse))

    private object Call0 {
      def unapply(arg: Tree)(implicit ctx: Context): Option[(RefTree, List[List[Tree]])] = arg match {
        case Select(Call0(fn, args), nme.apply) if defn.isImplicitFunctionType(fn.tpe.widenDealias.finalResultType) =>
          Some((fn, args))
        case fn: RefTree => Some((fn, Nil))
        case Apply(f @ Call0(fn, args1), args2) =>
          if (f.tpe.widenDealias.isErasedMethod) Some((fn, args1))
          else Some((fn, args2 :: args1))
        case TypeApply(Call0(fn, args), _) => Some((fn, args))
        case _ => None
      }
    }
  }
}

