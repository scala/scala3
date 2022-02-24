package dotty.tools.dotc
package transform

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.{InvocationTargetException, Method => JLRMethod}

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.SymDenotations.PrefixSeparator
import dotty.tools.dotc.core.Denotations.staticRef
import dotty.tools.dotc.core.TypeErasure
import dotty.tools.dotc.core.Constants.Constant

import scala.util.control.NonFatal
import dotty.tools.dotc.util.SrcPos
import dotty.tools.repl.AbstractFileClassLoader

import scala.reflect.ClassTag

import dotty.tools.dotc.quoted.{PickledQuotes, QuoteUtils}

import scala.quoted.Quotes
import scala.quoted.runtime.impl._

/** Utility class to splice quoted expressions */
object Splicer {
  import tpd._

  /** Splice the Tree for a Quoted expression. `${'{xyz}}` becomes `xyz`
   *  and for `$xyz` the tree of `xyz` is interpreted for which the
   *  resulting expression is returned as a `Tree`
   *
   *  See: `Staging`
   */
  def splice(tree: Tree, splicePos: SrcPos, spliceExpansionPos: SrcPos, classLoader: ClassLoader)(using Context): Tree = tree match {
    case Quoted(quotedTree) => quotedTree
    case _ =>
      val macroOwner = newSymbol(ctx.owner, nme.MACROkw, Macro | Synthetic, defn.AnyType, coord = tree.span)
      try
        val sliceContext = SpliceScope.contextWithNewSpliceScope(splicePos.sourcePos).withOwner(macroOwner)
        inContext(sliceContext) {
          val oldContextClassLoader = Thread.currentThread().getContextClassLoader
          Thread.currentThread().setContextClassLoader(classLoader)
          try {
            val interpreter = new Interpreter(splicePos, classLoader)

            // Some parts of the macro are evaluated during the unpickling performed in quotedExprToTree
            val interpretedExpr = interpreter.interpret[Quotes => scala.quoted.Expr[Any]](tree)
            val interpretedTree = interpretedExpr.fold(tree)(macroClosure => PickledQuotes.quotedExprToTree(macroClosure(QuotesImpl())))

            checkEscapedVariables(interpretedTree, macroOwner)
          } finally {
            Thread.currentThread().setContextClassLoader(oldContextClassLoader)
          }
        }.changeOwner(macroOwner, ctx.owner)
      catch {
        case ex: CompilationUnit.SuspendException =>
          throw ex
        case ex: scala.quoted.runtime.StopMacroExpansion =>
          if !ctx.reporter.hasErrors then
            report.error("Macro expansion was aborted by the macro without any errors reported. Macros should issue errors to end-users to facilitate debugging when aborting a macro expansion.", splicePos)
          // errors have been emitted
          EmptyTree
        case ex: StopInterpretation =>
          report.error(ex.msg, ex.pos)
          ref(defn.Predef_undefined).withType(ErrorType(ex.msg))
        case NonFatal(ex) =>
          val msg =
            s"""Failed to evaluate macro.
               |  Caused by ${ex.getClass}: ${if (ex.getMessage == null) "" else ex.getMessage}
               |    ${ex.getStackTrace.takeWhile(_.getClassName != "dotty.tools.dotc.transform.Splicer$").drop(1).mkString("\n    ")}
             """.stripMargin
          report.error(msg, spliceExpansionPos)
          ref(defn.Predef_undefined).withType(ErrorType(msg))
      }
  }

  /** Checks that no symbol that whas generated within the macro expansion has an out of scope reference */
  def checkEscapedVariables(tree: Tree, expansionOwner: Symbol)(using Context): tree.type =
    new TreeTraverser {
      private[this] var locals = Set.empty[Symbol]
      private def markSymbol(sym: Symbol)(using Context): Unit =
          locals = locals + sym
      private def markDef(tree: Tree)(using Context): Unit = tree match {
        case tree: DefTree => markSymbol(tree.symbol)
        case _ =>
      }
      def traverse(tree: Tree)(using Context): Unit =
        def traverseOver(lastEntered: Set[Symbol]) =
          try traverseChildren(tree)
          finally locals = lastEntered
        tree match
          case tree: Ident if isEscapedVariable(tree.symbol) =>
            val sym = tree.symbol
            report.error(em"While expanding a macro, a reference to $sym was used outside the scope where it was defined", tree.srcPos)
          case Block(stats, _) =>
            val last = locals
            stats.foreach(markDef)
            traverseOver(last)
          case CaseDef(pat, guard, body) =>
            val last = locals
            tpd.patVars(pat).foreach(markSymbol)
            traverseOver(last)
          case _ =>
            markDef(tree)
            traverseChildren(tree)
      private def isEscapedVariable(sym: Symbol)(using Context): Boolean =
        sym.exists && !sym.is(Package)
        && sym.owner.ownersIterator.exists(x =>
          x == expansionOwner || // symbol was generated within this macro expansion
          x.is(Macro, butNot = Method) && x.name == nme.MACROkw // symbol was generated within another macro expansion
        )
        && !locals.contains(sym) // symbol is not in current scope
    }.traverse(tree)
    tree


  /** Check that the Tree can be spliced. `${'{xyz}}` becomes `xyz`
    *  and for `$xyz` the tree of `xyz` is interpreted for which the
    *  resulting expression is returned as a `Tree`
    *
    *  See: `Staging`
    */
  def checkValidMacroBody(tree: Tree)(using Context): Unit = tree match {
    case Quoted(_) => // ok
    case _ =>
      type Env = Set[Symbol]

      def checkValidStat(tree: Tree)(using Env): Env = tree match {
        case tree: ValDef if tree.symbol.is(Synthetic) =>
          // Check val from `foo(j = x, i = y)` which it is expanded to
          // `val j$1 = x; val i$1 = y; foo(i = i$1, j = j$1)`
          checkIfValidArgument(tree.rhs)
          summon[Env] + tree.symbol
        case _ =>
          report.error("Macro should not have statements", tree.srcPos)
          summon[Env]
      }

      def checkIfValidArgument(tree: Tree)(using Env): Unit = tree match {
        case Block(Nil, expr) => checkIfValidArgument(expr)
        case Typed(expr, _) => checkIfValidArgument(expr)

        case Apply(Select(Apply(fn, quoted :: Nil), nme.apply), _) if fn.symbol == defn.QuotedRuntime_exprQuote =>
          val noSpliceChecker = new TreeTraverser {
            def traverse(tree: Tree)(using Context): Unit = tree match
              case Spliced(_) =>
                report.error("Quoted argument of macros may not have splices", tree.srcPos)
              case _ =>
                traverseChildren(tree)
          }
          noSpliceChecker.traverse(quoted)

        case Apply(TypeApply(fn, List(quoted)), _)if fn.symbol == defn.QuotedTypeModule_of =>
          // OK

        case Literal(Constant(value)) =>
          // OK

        case NamedArg(_, arg) =>
          checkIfValidArgument(arg)

        case SeqLiteral(elems, _) =>
          elems.foreach(checkIfValidArgument)

        case tree: Ident if summon[Env].contains(tree.symbol) || tree.symbol.is(Inline, butNot = Method) =>
          // OK

        case _ =>
          val extra = if tree.span.isZeroExtent then ": " + tree.show else ""
          report.error(
            s"""Malformed macro parameter$extra
              |
              |Parameters may only be:
              | * Quoted parameters or fields
              | * Literal values of primitive types
              | * References to `inline val`s
              |""".stripMargin, tree.srcPos)
      }

      def checkIfValidStaticCall(tree: Tree)(using Env): Unit = tree match {
        case closureDef(ddef @ DefDef(_, ValDefs(ev :: Nil) :: Nil, _, _)) if ddef.symbol.info.isContextualMethod =>
          checkIfValidStaticCall(ddef.rhs)(using summon[Env] + ev.symbol)

        case Block(stats, expr) =>
          val newEnv = stats.foldLeft(summon[Env])((env, stat) => checkValidStat(stat)(using env))
          checkIfValidStaticCall(expr)(using newEnv)

        case Typed(expr, _) =>
          checkIfValidStaticCall(expr)

        case Apply(Select(Apply(fn, quoted :: Nil), nme.apply), _) if fn.symbol == defn.QuotedRuntime_exprQuote =>
          // OK, canceled and warning emitted

        case Call(fn, args)
            if (fn.symbol.isConstructor && fn.symbol.owner.owner.is(Package)) ||
               fn.symbol.is(Module) || fn.symbol.isStatic ||
               (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic) =>
          if (fn.symbol.flags.is(Inline))
            report.error("Macro cannot be implemented with an `inline` method", fn.srcPos)
          args.flatten.foreach(checkIfValidArgument)

        case _ =>
          report.error(
            """Malformed macro.
              |
              |Expected the splice ${...} to contain a single call to a static method.
              |""".stripMargin, tree.srcPos)
      }

      checkIfValidStaticCall(tree)(using Set.empty)
  }

  /** Tree interpreter that evaluates the tree */
  private class Interpreter(pos: SrcPos, classLoader: ClassLoader)(using Context) {

    type Env = Map[Symbol, Object]

    /** Returns the interpreted result of interpreting the code a call to the symbol with default arguments.
     *  Return Some of the result or None if some error happen during the interpretation.
     */
    def interpret[T](tree: Tree)(implicit ct: ClassTag[T]): Option[T] =
      interpretTree(tree)(Map.empty) match {
        case obj: T => Some(obj)
        case obj =>
          // TODO upgrade to a full type tag check or something similar
          report.error(s"Interpreted tree returned a result of an unexpected type. Expected ${ct.runtimeClass} but was ${obj.getClass}", pos)
          None
      }

    def interpretTree(tree: Tree)(implicit env: Env): Object = tree match {
      case Apply(Select(Apply(TypeApply(fn, _), quoted :: Nil), nme.apply), _) if fn.symbol == defn.QuotedRuntime_exprQuote =>
        val quoted1 = quoted match {
          case quoted: Ident if quoted.symbol.isAllOf(InlineByNameProxy) =>
            // inline proxy for by-name parameter
            quoted.symbol.defTree.asInstanceOf[DefDef].rhs
          case Inlined(EmptyTree, _, quoted) => quoted
          case _ => quoted
        }
        interpretQuote(quoted1)

      case Apply(TypeApply(fn, quoted :: Nil), _) if fn.symbol == defn.QuotedTypeModule_of =>
        interpretTypeQuote(quoted)

      case Literal(Constant(value)) =>
        interpretLiteral(value)

      case tree: Ident if tree.symbol.is(Inline, butNot = Method) =>
        tree.tpe.widenTermRefExpr match
          case ConstantType(c) => c.value.asInstanceOf[Object]
          case _ => throw new StopInterpretation(em"${tree.symbol} could not be inlined", tree.srcPos)

      // TODO disallow interpreted method calls as arguments
      case Call(fn, args) =>
        if (fn.symbol.isConstructor && fn.symbol.owner.owner.is(Package))
          interpretNew(fn.symbol, args.flatten.map(interpretTree))
        else if (fn.symbol.is(Module))
          interpretModuleAccess(fn.symbol)
        else if (fn.symbol.is(Method) && fn.symbol.isStatic) {
          val staticMethodCall = interpretedStaticMethodCall(fn.symbol.owner, fn.symbol)
          staticMethodCall(interpretArgs(args, fn.symbol.info))
        }
        else if fn.symbol.isStatic then
          assert(args.isEmpty)
          interpretedStaticFieldAccess(fn.symbol)
        else if (fn.qualifier.symbol.is(Module) && fn.qualifier.symbol.isStatic)
          if (fn.name == nme.asInstanceOfPM)
            interpretModuleAccess(fn.qualifier.symbol)
          else {
            val staticMethodCall = interpretedStaticMethodCall(fn.qualifier.symbol.moduleClass, fn.symbol)
            staticMethodCall(interpretArgs(args, fn.symbol.info))
          }
        else if (env.contains(fn.symbol))
          env(fn.symbol)
        else if (tree.symbol.is(InlineProxy))
          interpretTree(tree.symbol.defTree.asInstanceOf[ValOrDefDef].rhs)
        else
          unexpectedTree(tree)

      case closureDef((ddef @ DefDef(_, ValDefs(arg :: Nil) :: Nil, _, _))) =>
        (obj: AnyRef) => interpretTree(ddef.rhs)(using env.updated(arg.symbol, obj))

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

    private def interpretArgs(argss: List[List[Tree]], fnType: Type)(using Env): List[Object] = {
      def interpretArgsGroup(args: List[Tree], argTypes: List[Type]): List[Object] =
        assert(args.size == argTypes.size)
        val view =
          for (arg, info) <- args.lazyZip(argTypes) yield
            info match
              case _: ExprType => () => interpretTree(arg) // by-name argument
              case _ => interpretTree(arg) // by-value argument
        view.toList

      fnType.dealias match
        case fnType: MethodType if fnType.isErasedMethod => interpretArgs(argss, fnType.resType)
        case fnType: MethodType =>
          val argTypes = fnType.paramInfos
          assert(argss.head.size == argTypes.size)
          interpretArgsGroup(argss.head, argTypes) ::: interpretArgs(argss.tail, fnType.resType)
        case fnType: AppliedType if defn.isContextFunctionType(fnType) =>
          val argTypes :+ resType = fnType.args
          interpretArgsGroup(argss.head, argTypes) ::: interpretArgs(argss.tail, resType)
        case fnType: PolyType => interpretArgs(argss, fnType.resType)
        case fnType: ExprType => interpretArgs(argss, fnType.resType)
        case _ =>
          assert(argss.isEmpty)
          Nil
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
      new ExprImpl(Inlined(EmptyTree, Nil, QuoteUtils.changeOwnerOfTree(tree, ctx.owner)).withSpan(tree.span), SpliceScope.getCurrent)

    private def interpretTypeQuote(tree: Tree)(implicit env: Env): Object =
      new TypeImpl(QuoteUtils.changeOwnerOfTree(tree, ctx.owner), SpliceScope.getCurrent)

    private def interpretLiteral(value: Any)(implicit env: Env): Object =
      value.asInstanceOf[Object]

    private def interpretVarargs(args: List[Object])(implicit env: Env): Object =
      args.toSeq

    private def interpretedStaticMethodCall(moduleClass: Symbol, fn: Symbol)(implicit env: Env): List[Object] => Object = {
      val (inst, clazz) =
        try
          if (moduleClass.name.startsWith(str.REPL_SESSION_LINE))
            (null, loadReplLineClass(moduleClass))
          else {
            val inst = loadModule(moduleClass)
            (inst, inst.getClass)
          }
        catch
          case MissingClassDefinedInCurrentRun(sym)  if ctx.compilationUnit.isSuspendable =>
            if (ctx.settings.XprintSuspension.value)
              report.echo(i"suspension triggered by a dependency on $sym", pos)
            ctx.compilationUnit.suspend() // this throws a SuspendException

      val name = fn.name.asTermName
      val method = getMethod(clazz, name, paramsSig(fn))
      (args: List[Object]) => stopIfRuntimeException(method.invoke(inst, args: _*), method)
    }

    private def interpretedStaticFieldAccess(sym: Symbol)(implicit env: Env): Object = {
      val clazz = loadClass(sym.owner.fullName.toString)
      val field = clazz.getField(sym.name.toString)
      field.get(null)
    }

    private def interpretModuleAccess(fn: Symbol)(implicit env: Env): Object =
      loadModule(fn.moduleClass)

    private def interpretNew(fn: Symbol, args: => List[Object])(implicit env: Env): Object = {
      val clazz = loadClass(fn.owner.fullName.toString)
      val constr = clazz.getConstructor(paramsSig(fn): _*)
      constr.newInstance(args: _*).asInstanceOf[Object]
    }

    private def unexpectedTree(tree: Tree)(implicit env: Env): Object =
      throw new StopInterpretation("Unexpected tree could not be interpreted: " + tree, tree.srcPos)

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

    private def getMethod(clazz: Class[?], name: Name, paramClasses: List[Class[?]]): JLRMethod =
      try clazz.getMethod(name.toString, paramClasses: _*)
      catch {
        case _: NoSuchMethodException =>
          val msg = em"Could not find method ${clazz.getCanonicalName}.$name with parameters ($paramClasses%, %)"
          throw new StopInterpretation(msg, pos)
        case MissingClassDefinedInCurrentRun(sym) if ctx.compilationUnit.isSuspendable =>
            if (ctx.settings.XprintSuspension.value)
              report.echo(i"suspension triggered by a dependency on $sym", pos)
            ctx.compilationUnit.suspend() // this throws a SuspendException
      }

    private def stopIfRuntimeException[T](thunk: => T, method: JLRMethod): T =
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
            case ex: scala.quoted.runtime.StopMacroExpansion =>
              throw ex
            case MissingClassDefinedInCurrentRun(sym) if ctx.compilationUnit.isSuspendable =>
              if (ctx.settings.XprintSuspension.value)
                report.echo(i"suspension triggered by a dependency on $sym", pos)
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
      def unapply(targetException: NoClassDefFoundError)(using Context): Option[Symbol] = {
        val className = targetException.getMessage
        if (className eq null) None
        else {
          val sym = staticRef(className.toTypeName).symbol
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
            packageName + tpe.classSymbol.fullNameSeparated(PrefixSeparator.Flat).toString
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
        case tp: AppliedType if defn.isContextFunctionType(tp) =>
          // Call context function type direct method
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
  private class StopInterpretation(val msg: String, val pos: SrcPos) extends Exception

  object Call {
    /** Matches an expression that is either a field access or an application
     *  It retruns a TermRef containing field accessed or a method reference and the arguments passed to it.
     */
    def unapply(arg: Tree)(using Context): Option[(RefTree, List[List[Tree]])] =
      Call0.unapply(arg).map((fn, args) => (fn, args.reverse))

    private object Call0 {
      def unapply(arg: Tree)(using Context): Option[(RefTree, List[List[Tree]])] = arg match {
        case Select(Call0(fn, args), nme.apply) if defn.isContextFunctionType(fn.tpe.widenDealias.finalResultType) =>
          Some((fn, args))
        case fn: Ident => Some((tpd.desugarIdent(fn).withSpan(fn.span), Nil))
        case fn: Select => Some((fn, Nil))
        case Apply(f @ Call0(fn, args1), args2) =>
          if (f.tpe.widenDealias.isErasedMethod) Some((fn, args1))
          else Some((fn, args2 :: args1))
        case TypeApply(Call0(fn, args), _) => Some((fn, args))
        case _ => None
      }
    }
  }
}
