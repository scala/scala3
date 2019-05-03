package dotty.tools.backend.sjs

import scala.annotation.switch

import scala.collection.mutable

import dotty.tools.FatalError

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Phases.Phase

import dotty.tools.dotc.core._
import Periods._
import SymDenotations._
import Contexts._
import Decorators._
import Flags._
import dotty.tools.dotc.ast.Trees._
import Names._
import Types._
import Symbols._
import Denotations._
import Phases._
import StdNames._

import dotty.tools.dotc.transform.Erasure

import org.scalajs.ir
import org.scalajs.ir.{ClassKind, Position, Trees => js, Types => jstpe}
import js.OptimizerHints

import JSEncoding._
import JSInterop._
import ScopedVar.withScopedVars

/** Main codegen for Scala.js IR.
 *
 *  [[GenSJSIR]] creates one instance of `JSCodeGen` per compilation unit.
 *  The `run()` method processes the whole compilation unit and generates
 *  `.sjsir` files for it.
 *
 *  There are 4 main levels of translation:
 *
 *  - `genCompilationUnit()` iterates through all the type definitions in the
 *    compilation unit. Each generated `js.ClassDef` is serialized to an
 *    `.sjsir` file.
 *  - `genScalaClass()` and other similar methods generate the skeleton of
 *    classes.
 *  - `genMethod()` and similar methods generate the declarations of methods.
 *  - `genStatOrExpr()` and everything else generate the bodies of methods.
 */
class JSCodeGen()(implicit ctx: Context) {
  import JSCodeGen._
  import tpd._

  private val jsdefn = JSDefinitions.jsdefn
  private val primitives = new JSPrimitives(ctx)

  private val positionConversions = new JSPositions()(ctx)
  import positionConversions.{pos2irPos, implicitPos2irPos}

  // Some state --------------------------------------------------------------

  private val currentClassSym = new ScopedVar[Symbol]
  private val currentMethodSym = new ScopedVar[Symbol]
  private val localNames = new ScopedVar[LocalNameGenerator]
  private val thisLocalVarIdent = new ScopedVar[Option[js.Ident]]
  private val undefinedDefaultParams = new ScopedVar[mutable.Set[Symbol]]

  private def withNewLocalNameScope[A](body: => A): A = {
    withScopedVars(localNames := new LocalNameGenerator) {
      body
    }
  }

  /** Implicitly materializes the current local name generator. */
  private implicit def implicitLocalNames: LocalNameGenerator = localNames.get

  /* See genSuperCall()
   * TODO Can we avoid this unscoped var?
   */
  private[this] var isModuleInitialized: Boolean = false

  private def currentClassType = encodeClassType(currentClassSym)

  /** Returns a new fresh local identifier. */
  private def freshLocalIdent()(implicit pos: Position): js.Ident =
    localNames.get.freshLocalIdent()

  /** Returns a new fresh local identifier. */
  private def freshLocalIdent(base: String)(implicit pos: Position): js.Ident =
    localNames.get.freshLocalIdent(base)

  /** Returns a new fresh local identifier. */
  private def freshLocalIdent(base: TermName)(implicit pos: Position): js.Ident =
    localNames.get.freshLocalIdent(base)

  // Compilation unit --------------------------------------------------------

  def run(): Unit = {
    genCompilationUnit(ctx.compilationUnit)
  }

  /** Generates the Scala.js IR for a compilation unit
   *  This method iterates over all the class and interface definitions
   *  found in the compilation unit and emits their IR (.sjsir).
   *
   *  Some classes are never actually emitted:
   *  - Classes representing primitive types
   *  - The scala.Array class
   *
   *  TODO Some classes representing anonymous functions are not actually emitted.
   *  Instead, a temporary representation of their `apply` method is built
   *  and recorded, so that it can be inlined as a JavaScript anonymous
   *  function in the method that instantiates it.
   *
   *  Other ClassDefs are emitted according to their nature:
   *  * Scala.js-defined JS class     -> `genScalaJSDefinedJSClass()`
   *  * Other raw JS type (<: js.Any) -> `genRawJSClassData()`
   *  * Interface                     -> `genInterface()`
   *  * Normal class                  -> `genClass()`
   */
  private def genCompilationUnit(cunit: CompilationUnit): Unit = {
    def collectTypeDefs(tree: Tree): List[TypeDef] = {
      tree match {
        case EmptyTree            => Nil
        case PackageDef(_, stats) => stats.flatMap(collectTypeDefs)
        case cd: TypeDef          => cd :: Nil
        case _: ValDef            => Nil // module instance
      }
    }
    val allTypeDefs = collectTypeDefs(cunit.tpdTree)

    val generatedClasses = mutable.ListBuffer.empty[(Symbol, js.ClassDef)]

    // TODO Record anonymous JS function classes

    /* Finally, we emit true code for the remaining class defs. */
    for (td <- allTypeDefs) {
      val sym = td.symbol
      implicit val pos: Position = sym.span

      /* Do not actually emit code for primitive types nor scala.Array. */
      val isPrimitive =
        sym.isPrimitiveValueClass || sym == defn.ArrayClass

      if (!isPrimitive) {
        withScopedVars(
            currentClassSym := sym
        ) {
          val tree = if (isJSType(sym)) {
            /*assert(!isRawJSFunctionDef(sym),
                s"Raw JS function def should have been recorded: $cd")*/
            if (!sym.is(Trait) && isScalaJSDefinedJSClass(sym))
              genScalaJSDefinedJSClass(td)
            else
              genRawJSClassData(td)
          } else if (sym.is(Trait)) {
            genInterface(td)
          } else {
            genScalaClass(td)
          }

          generatedClasses += ((sym, tree))
        }
      }
    }

    val clDefs = generatedClasses.map(_._2).toList

    for ((sym, tree) <- generatedClasses)
      genIRFile(cunit, sym, tree)
  }

  private def genIRFile(cunit: CompilationUnit, sym: Symbol,
      tree: ir.Trees.ClassDef): Unit = {
    val outfile = getFileFor(cunit, sym, ".sjsir")
    val output = outfile.bufferedOutput
    try {
      ir.Serializers.serialize(output, tree)
    } finally {
      output.close()
    }
  }

  private def getFileFor(cunit: CompilationUnit, sym: Symbol,
      suffix: String): dotty.tools.io.AbstractFile = {
    import dotty.tools.io._

    val outputDirectory: AbstractFile =
      ctx.settings.outputDir.value

    val pathParts = sym.fullName.toString.split("[./]")
    val dir = (outputDirectory /: pathParts.init)(_.subdirectoryNamed(_))

    var filename = pathParts.last
    if (sym.is(ModuleClass))
      filename = filename + nme.MODULE_SUFFIX.toString

    dir fileNamed (filename + suffix)
  }

  // Generate a class --------------------------------------------------------

  /** Gen the IR ClassDef for a Scala class definition (maybe a module class).
   */
  private def genScalaClass(td: TypeDef): js.ClassDef = {
    val sym = td.symbol.asClass
    implicit val pos: Position = sym.span

    assert(!sym.is(Trait),
        "genScalaClass() must be called only for normal classes: "+sym)
    assert(sym.superClass != NoSymbol, sym)

    /*if (hasDefaultCtorArgsAndRawJSModule(sym)) {
      reporter.error(pos,
          "Implementation restriction: constructors of " +
          "Scala classes cannot have default parameters " +
          "if their companion module is JS native.")
    }*/

    val classIdent = encodeClassFullNameIdent(sym)
    val isHijacked = false //isHijackedBoxedClass(sym)

    // Optimizer hints

    def isStdLibClassWithAdHocInlineAnnot(sym: Symbol): Boolean = {
      val fullName = sym.fullName.toString
      (fullName.startsWith("scala.Tuple") && !fullName.endsWith("$")) ||
      (fullName.startsWith("scala.collection.mutable.ArrayOps$of"))
    }

    val shouldMarkInline = (
        sym.hasAnnotation(jsdefn.InlineAnnot) ||
        (sym.isAnonymousFunction && !sym.isSubClass(defn.PartialFunctionClass)) ||
        isStdLibClassWithAdHocInlineAnnot(sym))

    val optimizerHints = {
      OptimizerHints.empty
        .withInline(shouldMarkInline)
        .withNoinline(sym.hasAnnotation(jsdefn.NoinlineAnnot))
    }

    // Generate members (constructor + methods)

    val generatedMethods = new mutable.ListBuffer[js.MethodDef]
    val exportedSymbols = new mutable.ListBuffer[Symbol]

    val tpl = td.rhs.asInstanceOf[Template]
    for (tree <- tpl.constr :: tpl.body) {
      tree match {
        case EmptyTree => ()

        case _: ValDef =>
          () // fields are added via genClassFields()

        case dd: DefDef =>
          val sym = dd.symbol

          val isExport = false //jsInterop.isExport(sym)
          val isNamedExport = false /*isExport && sym.annotations.exists(
              _.symbol == JSExportNamedAnnotation)*/

          /*if (isNamedExport)
            generatedMethods += genNamedExporterDef(dd)
          else*/
          generatedMethods ++= genMethod(dd)

          if (isExport) {
            // We add symbols that we have to export here. This way we also
            // get inherited stuff that is implemented in this class.
            exportedSymbols += sym
          }

        case _ =>
          throw new FatalError("Illegal tree in body of genScalaClass(): " + tree)
      }
    }

    // Generate fields and add to methods + ctors
    val generatedMembers = genClassFields(td) ++ generatedMethods.toList

    // Generate the exported members, constructors and accessors
    val exports = {
      /*
      // Generate the exported members
      val memberExports = genMemberExports(sym, exportedSymbols.toList)

      // Generate exported constructors or accessors
      val exportedConstructorsOrAccessors =
        if (isStaticModule(sym)) genModuleAccessorExports(sym)
        else genConstructorExports(sym)

      memberExports ++ exportedConstructorsOrAccessors
      */
      Nil
    }

    // Static initializer
    val optStaticInitializer = {
      // Initialization of reflection data, if required
      val reflectInit = {
        val enableReflectiveInstantiation = {
          sym.baseClasses.exists { ancestor =>
            ancestor.hasAnnotation(jsdefn.EnableReflectiveInstantiationAnnot)
          }
        }
        if (enableReflectiveInstantiation)
          genRegisterReflectiveInstantiation(sym)
        else
          None
      }

      val staticInitializerStats = reflectInit.toList
      if (staticInitializerStats.nonEmpty)
        Some(genStaticInitializerWithStats(js.Block(staticInitializerStats)))
      else
        None
    }

    // Hashed definitions of the class
    val hashedDefs =
      ir.Hashers.hashMemberDefs(generatedMembers ++ exports ++ optStaticInitializer)

    // The complete class definition
    val kind =
      if (isStaticModule(sym)) ClassKind.ModuleClass
      else if (isHijacked) ClassKind.HijackedClass
      else ClassKind.Class

    val classDefinition = js.ClassDef(
        classIdent,
        kind,
        None,
        Some(encodeClassFullNameIdent(sym.superClass)),
        genClassInterfaces(sym),
        None,
        None,
        hashedDefs,
        Nil)(
        optimizerHints)

    classDefinition
  }

  /** Gen the IR ClassDef for a Scala.js-defined JS class. */
  private def genScalaJSDefinedJSClass(td: TypeDef): js.ClassDef = {
    ???
  }

  /** Gen the IR ClassDef for a raw JS class or trait.
   */
  private def genRawJSClassData(td: TypeDef): js.ClassDef = {
    val sym = td.symbol.asClass
    implicit val pos: Position = sym.span

    val classIdent = encodeClassFullNameIdent(sym)
    val kind = {
      if (sym.is(Trait)) ClassKind.AbstractJSType
      else if (sym.is(ModuleClass)) ClassKind.NativeJSModuleClass
      else ClassKind.NativeJSClass
    }
    val superClass =
      if (sym.is(Trait)) None
      else Some(encodeClassFullNameIdent(sym.superClass))
    val jsNativeLoadSpec = {
      if (sym.is(Trait)) None
      else {
        val path = fullJSNameOf(sym).split('.').toList
        Some(js.JSNativeLoadSpec.Global(path.head, path.tail))
      }
    }

    js.ClassDef(
        classIdent,
        kind,
        None,
        superClass,
        genClassInterfaces(sym),
        None,
        jsNativeLoadSpec,
        Nil,
        Nil)(
        OptimizerHints.empty)
  }

  /** Gen the IR ClassDef for an interface definition.
   */
  private def genInterface(td: TypeDef): js.ClassDef = {
    val sym = td.symbol.asClass
    implicit val pos: Position = sym.span

    val classIdent = encodeClassFullNameIdent(sym)

    val generatedMethods = new mutable.ListBuffer[js.MethodDef]

    val tpl = td.rhs.asInstanceOf[Template]
    for (tree <- tpl.constr :: tpl.body) {
      tree match {
        case EmptyTree  => ()
        case dd: DefDef => generatedMethods ++= genMethod(dd)
        case _ =>
          throw new FatalError("Illegal tree in gen of genInterface(): " + tree)
      }
    }

    val superInterfaces = genClassInterfaces(sym)

    // Hashed definitions of the interface
    val hashedDefs =
      ir.Hashers.hashMemberDefs(generatedMethods.toList)

    js.ClassDef(
        classIdent,
        ClassKind.Interface,
        None,
        None,
        superInterfaces,
        None,
        None,
        hashedDefs,
        Nil)(
        OptimizerHints.empty)
  }

  private def genClassInterfaces(sym: ClassSymbol)(
      implicit pos: Position): List[js.Ident] = {
    import dotty.tools.dotc.transform.SymUtils._
    for {
      intf <- sym.directlyInheritedTraits
    } yield {
      encodeClassFullNameIdent(intf)
    }
  }

  // Generate the fields of a class ------------------------------------------

  /** Gen definitions for the fields of a class.
   */
  private def genClassFields(td: TypeDef): List[js.FieldDef] = {
    val classSym = td.symbol.asClass
    assert(currentClassSym.get == classSym,
        "genClassFields called with a ClassDef other than the current one")

    // Non-method term members are fields
    classSym.info.decls.filter(f => !f.is(Method) && f.isTerm).map({ f =>
      implicit val pos = f.span

      val name =
        /*if (isExposed(f)) js.StringLiteral(jsNameOf(f))
        else*/ encodeFieldSym(f)

      val irTpe = //if (!isScalaJSDefinedJSClass(classSym)) {
        toIRType(f.info)
      /*} else {
        val tpeEnteringPosterasure =
          enteringPhase(currentRun.posterasurePhase)(f.tpe)
        tpeEnteringPosterasure match {
          case tpe: ErasedValueType =>
            /* Here, we must store the field as the boxed representation of
             * the value class. The default value of that field, as
             * initialized at the time the instance is created, will
             * therefore be null. This will not match the behavior we would
             * get in a Scala class. To match the behavior, we would need to
             * initialized to an instance of the boxed representation, with
             * an underlying value set to the zero of its type. However we
             * cannot implement that, so we live with the discrepancy.
             * Anyway, scalac also has problems with uninitialized value
             * class values, if they come from a generic context.
             *
             * TODO Evaluate how much of this needs to be adapted for dotc,
             * which unboxes `null` to the zero of their underlying.
             */
            jstpe.ClassType(encodeClassFullName(tpe.valueClazz))

          case _ if f.tpe.typeSymbol == CharClass =>
            /* Will be initialized to null, which will unbox to '\0' when
             * read.
             */
            jstpe.ClassType(ir.Definitions.BoxedCharacterClass)

          case _ =>
            /* Other types are not boxed, so we can initialized them to
             * their true zero.
             */
            toIRType(f.tpe)
        }
      }*/

      val flags = js.MemberFlags.empty.withMutable(f.is(Mutable))
      js.FieldDef(flags, name, irTpe)
    }).toList
  }

  // Static initializers -----------------------------------------------------

  private def genStaticInitializerWithStats(stats: js.Tree)(
      implicit pos: Position): js.MethodDef = {
    js.MethodDef(
        js.MemberFlags.empty.withNamespace(js.MemberNamespace.StaticConstructor),
        js.Ident(ir.Definitions.StaticInitializerName),
        Nil,
        jstpe.NoType,
        Some(stats))(
        OptimizerHints.empty, None)
  }

  private def genRegisterReflectiveInstantiation(sym: Symbol)(
      implicit pos: Position): Option[js.Tree] = {
    if (isStaticModule(sym))
      genRegisterReflectiveInstantiationForModuleClass(sym)
    else if (sym.is(ModuleClass))
      None // scala-js#3228
    else if (sym.is(Lifted) && !sym.originalOwner.isClass)
      None // scala-js#3227
    else
      genRegisterReflectiveInstantiationForNormalClass(sym)
  }

  private def genRegisterReflectiveInstantiationForModuleClass(sym: Symbol)(
      implicit pos: Position): Option[js.Tree] = {
    val fqcnArg = js.StringLiteral(sym.fullName.toString)
    val runtimeClassArg = js.ClassOf(toTypeRef(sym.info))
    val loadModuleFunArg =
      js.Closure(arrow = true, Nil, Nil, genLoadModule(sym), Nil)

    val stat = genApplyMethod(
        genLoadModule(jsdefn.ReflectModule),
        jsdefn.Reflect_registerLoadableModuleClass,
        List(fqcnArg, runtimeClassArg, loadModuleFunArg))

    Some(stat)
  }

  private def genRegisterReflectiveInstantiationForNormalClass(sym: Symbol)(
      implicit pos: Position): Option[js.Tree] = {
    val ctors =
      if (sym.is(Abstract)) Nil
      else sym.info.member(nme.CONSTRUCTOR).alternatives.map(_.symbol).filter(m => !m.is(Private | Protected))

    if (ctors.isEmpty) {
      None
    } else {
      val constructorsInfos = for {
        ctor <- ctors
      } yield {
        withNewLocalNameScope {
          val (parameterTypes, formalParams, actualParams) = (for {
            (paramName, paramInfo) <- ctor.info.paramNamess.flatten.zip(ctor.info.paramInfoss.flatten)
          } yield {
            val paramType = js.ClassOf(toTypeRef(paramInfo))
            val paramDef = js.ParamDef(freshLocalIdent(paramName), jstpe.AnyType,
                mutable = false, rest = false)
            val actualParam = unbox(paramDef.ref, paramInfo)
            (paramType, paramDef, actualParam)
          }).unzip3

          val paramTypesArray = js.JSArrayConstr(parameterTypes)

          val newInstanceFun = js.Closure(arrow = true, Nil, formalParams, {
            js.New(encodeClassRef(sym), encodeMethodSym(ctor), actualParams)
          }, Nil)

          js.JSArrayConstr(List(paramTypesArray, newInstanceFun))
        }
      }

      val fqcnArg = js.StringLiteral(sym.fullName.toString)
      val runtimeClassArg = js.ClassOf(toTypeRef(sym.info))
      val ctorsInfosArg = js.JSArrayConstr(constructorsInfos)

      val stat = genApplyMethod(
          genLoadModule(jsdefn.ReflectModule),
          jsdefn.Reflect_registerInstantiatableClass,
          List(fqcnArg, runtimeClassArg, ctorsInfosArg))

      Some(stat)
    }
  }

  // Generate a method -------------------------------------------------------

  private def genMethod(dd: DefDef): Option[js.MethodDef] = {
    withScopedVars(
        localNames := new LocalNameGenerator
    ) {
      genMethodWithCurrentLocalNameScope(dd)
    }
  }

  /** Gen JS code for a method definition in a class or in an impl class.
   *  On the JS side, method names are mangled to encode the full signature
   *  of the Scala method, as described in `JSEncoding`, to support
   *  overloading.
   *
   *  Some methods are not emitted at all:
   *  - Primitives, since they are never actually called
   *  - Constructors of hijacked classes
   *
   *  Constructors are emitted by generating their body as a statement.
   *
   *  Other (normal) methods are emitted with `genMethodBody()`.
   */
  private def genMethodWithCurrentLocalNameScope(dd: DefDef): Option[js.MethodDef] = {
    implicit val pos = dd.span
    val sym = dd.symbol
    val vparamss = dd.vparamss
    val rhs = dd.rhs

    isModuleInitialized = false

    withScopedVars(
        currentMethodSym       := sym,
        undefinedDefaultParams := mutable.Set.empty,
        thisLocalVarIdent      := None
    ) {
      assert(vparamss.isEmpty || vparamss.tail.isEmpty,
          "Malformed parameter list: " + vparamss)
      val params = if (vparamss.isEmpty) Nil else vparamss.head.map(_.symbol)

      val isJSClassConstructor =
        sym.isClassConstructor && isScalaJSDefinedJSClass(currentClassSym)

      val methodName: js.PropertyName = encodeMethodSym(sym)

      def jsParams = for (param <- params) yield {
        implicit val pos = param.span
        js.ParamDef(encodeLocalSym(param), toIRType(param.info),
            mutable = false, rest = false)
      }

      /*if (primitives.isPrimitive(sym)) {
        None
      } else*/ if (sym.is(Deferred)) {
        Some(js.MethodDef(js.MemberFlags.empty, methodName,
            jsParams, toIRType(patchedResultType(sym)), None)(
            OptimizerHints.empty, None))
      } else /*if (isJSNativeCtorDefaultParam(sym)) {
        None
      } else if (sym.isClassConstructor && isHijackedBoxedClass(sym.owner)) {
        None
      } else*/ {
        /*def isTraitImplForwarder = dd.rhs match {
          case app: Apply => foreignIsImplClass(app.symbol.owner)
          case _          => false
        }*/

        val shouldMarkInline = {
          sym.hasAnnotation(jsdefn.InlineAnnot) ||
          sym.isAnonymousFunction
        }

        val shouldMarkNoinline = {
          sym.hasAnnotation(jsdefn.NoinlineAnnot) /*&&
          !isTraitImplForwarder*/
        }

        val optimizerHints = {
          OptimizerHints.empty
            .withInline(shouldMarkInline)
            .withNoinline(shouldMarkNoinline)
        }

        val methodDef = {
          /*if (isJSClassConstructor) {
            val body0 = genStat(rhs)
            val body1 =
              if (!sym.isPrimaryConstructor) body0
              else moveAllStatementsAfterSuperConstructorCall(body0)
            js.MethodDef(js.MemberFlags.empty, methodName,
                jsParams, jstpe.NoType, body1)(optimizerHints, None)
          } else*/ if (sym.isClassConstructor) {
            val namespace = js.MemberNamespace.Constructor
            js.MethodDef(js.MemberFlags.empty.withNamespace(namespace),
                methodName, jsParams, jstpe.NoType,
                Some(genStat(rhs)))(optimizerHints, None)
          } else {
            val namespace =
              if (sym.isPrivate) js.MemberNamespace.Private
              else js.MemberNamespace.Public
            val resultIRType = toIRType(patchedResultType(sym))
            genMethodDef(namespace, methodName,
                params, resultIRType, rhs, optimizerHints)
          }
        }

        Some(methodDef)
      }
    }
  }

  /** Generates the MethodDef of a (non-constructor) method
   *
   *  Most normal methods are emitted straightforwardly. If the result
   *  type is Unit, then the body is emitted as a statement. Otherwise, it is
   *  emitted as an expression.
   *
   *  Methods Scala.js-defined JS classes are compiled as static methods taking
   *  an explicit parameter for their `this` value.
   */
  private def genMethodDef(namespace: js.MemberNamespace, methodName: js.PropertyName,
      paramsSyms: List[Symbol], resultIRType: jstpe.Type,
      tree: Tree, optimizerHints: OptimizerHints): js.MethodDef = {
    implicit val pos = tree.span

    ctx.debuglog("genMethod " + methodName.encodedName)
    ctx.debuglog("")

    val jsParams = for (param <- paramsSyms) yield {
      implicit val pos = param.span
      js.ParamDef(encodeLocalSym(param), toIRType(param.info),
          mutable = false, rest = false)
    }

    def genBody() =
      if (resultIRType == jstpe.NoType) genStat(tree)
      else genExpr(tree)

    //if (!isScalaJSDefinedJSClass(currentClassSym)) {
      val flags = js.MemberFlags.empty.withNamespace(namespace)
      js.MethodDef(flags, methodName, jsParams, resultIRType, Some(genBody()))(
          optimizerHints, None)
    /*} else {
      assert(!namespace.isStatic, tree.span)

      withScopedVars(
        thisLocalVarIdent := Some(freshLocalIdent("this"))
      ) {
        val thisParamDef = js.ParamDef(thisLocalVarIdent.get.get,
            jstpe.AnyType, mutable = false, rest = false)

        js.MethodDef(static = true, methodName, thisParamDef :: jsParams,
            resultIRType, genBody())(
            optimizerHints, None)
      }
    }*/
  }

  // Generate statements and expressions -------------------------------------

  /** Gen JS code for a tree in statement position (in the IR).
   */
  private def genStat(tree: Tree): js.Tree = {
    exprToStat(genStatOrExpr(tree, isStat = true))
  }

  /** Turn a JavaScript expression of type Unit into a statement */
  private def exprToStat(tree: js.Tree): js.Tree = {
    /* Any JavaScript expression is also a statement, but at least we get rid
     * of some pure expressions that come from our own codegen.
     */
    implicit val pos = tree.pos
    tree match {
      case js.Block(stats :+ expr)  => js.Block(stats :+ exprToStat(expr))
      case _:js.Literal | js.This() => js.Skip()
      case _                        => tree
    }
  }

  /** Gen JS code for a tree in expression position (in the IR).
   */
  private def genExpr(tree: Tree): js.Tree = {
    val result = genStatOrExpr(tree, isStat = false)
    assert(result.tpe != jstpe.NoType,
        s"genExpr($tree) returned a tree with type NoType at pos ${tree.span}")
    result
  }

  /** Gen JS code for a tree in statement or expression position (in the IR).
   *
   *  This is the main transformation method. Each node of the Scala AST
   *  is transformed into an equivalent portion of the JS AST.
   */
  private def genStatOrExpr(tree: Tree, isStat: Boolean): js.Tree = {
    implicit val pos = tree.span

    ctx.debuglog("  " + tree)
    ctx.debuglog("")

    tree match {
      /** Local val or var declaration */
      case tree @ ValDef(name, _, _) =>
        /* Must have been eliminated by the tail call transform performed
         * by genMethodBody(). */
        assert(name != nme.THIS,
            s"ValDef(_, nme.THIS, _, _) found at ${tree.span}")

        val sym = tree.symbol
        val rhs = tree.rhs
        val rhsTree = genExpr(rhs)

        rhsTree match {
          case js.Transient(UndefinedParam) =>
            /* This is an intermediate assignment for default params on a
             * js.Any. Add the symbol to the corresponding set to inform
             * the Ident resolver how to replace it and don't emit the symbol.
             */
            undefinedDefaultParams += sym
            js.Skip()
          case _ =>
            js.VarDef(encodeLocalSym(sym),
                toIRType(sym.info), sym.is(Mutable), rhsTree)
        }

      case If(cond, thenp, elsep) =>
        js.If(genExpr(cond), genStatOrExpr(thenp, isStat),
            genStatOrExpr(elsep, isStat))(toIRType(tree.tpe))

      case Labeled(bind, expr) =>
        js.Labeled(encodeLabelSym(bind.symbol), toIRType(tree.tpe), genStatOrExpr(expr, isStat))

      case Return(expr, from) =>
        val fromSym = from.symbol
        val label =
          if (fromSym.is(Label)) encodeLabelSym(fromSym)
          else localNames.get.getEnclosingReturnLabel()
        js.Return(toIRType(expr.tpe) match {
          case jstpe.NoType => js.Block(genStat(expr), js.Undefined())
          case _            => genExpr(expr)
        }, label)

      case WhileDo(cond, body) =>
        js.While(genExpr(cond), genStat(body))

      /*case t: Try =>
        genTry(t, isStat)*/

      case app: Apply =>
        genApply(app, isStat)

      case app: TypeApply =>
        genTypeApply(app)

      /*case app: ApplyDynamic =>
        genApplyDynamic(app)*/

      case tree: This =>
        if (tree.symbol == currentClassSym.get) {
          genThis()
        } else {
          assert(tree.symbol.is(Module),
              "Trying to access the this of another class: " +
              "tree.symbol = " + tree.symbol +
              ", class symbol = " + currentClassSym.get +
              " span:" + pos)
          genLoadModule(tree.symbol)
        }

      case Select(qualifier, _) =>
        val sym = tree.symbol
        if (sym.is(Module)) {
          assert(!sym.is(Package), "Cannot use package as value: " + tree)
          genLoadModule(sym)
        } else if (sym.is(JavaStatic)) {
          genLoadStaticField(sym)
        } else /*if (paramAccessorLocals contains sym) {
          paramAccessorLocals(sym).ref
        } else if (isScalaJSDefinedJSClass(sym.owner)) {
          val genQual = genExpr(qualifier)
          val boxed = if (isExposed(sym))
            js.JSBracketSelect(genQual, js.StringLiteral(jsNameOf(sym)))
          else
            js.JSDotSelect(genQual, encodeFieldSym(sym))
          fromAny(boxed,
              enteringPhase(currentRun.posterasurePhase)(sym.tpe))
        } else*/ {
          js.Select(genExpr(qualifier),
              encodeFieldSym(sym))(toIRType(sym.info))
        }

      case tree: Ident =>
        desugarIdent(tree).fold[js.Tree] {
          val sym = tree.symbol
          assert(!sym.is(Package), "Cannot use package as value: " + tree)
          if (sym.is(Module)) {
            genLoadModule(sym)
          } else if (undefinedDefaultParams.contains(sym)) {
            /* This is a default parameter whose assignment was moved to
             * a local variable. Put an undefined param instead.
             */
            js.Transient(UndefinedParam)(toIRType(sym.info))
          } else {
            js.VarRef(encodeLocalSym(sym))(toIRType(sym.info))
          }
        } { select =>
          genStatOrExpr(select, isStat)
        }

      case Literal(value) =>
        import Constants._
        value.tag match {
          case UnitTag =>
            js.Skip()
          case BooleanTag =>
            js.BooleanLiteral(value.booleanValue)
          case ByteTag | ShortTag | CharTag | IntTag =>
            js.IntLiteral(value.intValue)
          case LongTag =>
            js.LongLiteral(value.longValue)
          case FloatTag =>
            js.FloatLiteral(value.floatValue)
          case DoubleTag =>
            js.DoubleLiteral(value.doubleValue)
          case StringTag =>
            js.StringLiteral(value.stringValue)
          case NullTag =>
            js.Null()
          case ClazzTag =>
            genClassConstant(value.typeValue)
          /*case EnumTag =>
            genStaticMember(value.symbolValue)*/
        }

      case Block(stats, expr) =>
        js.Block(stats.map(genStat) :+ genStatOrExpr(expr, isStat))

      case Typed(expr, _) =>
        expr match {
          case _: Super => genThis()
          case _        => genExpr(expr)
        }

      case Assign(lhs0, rhs) =>
        val sym = lhs0.symbol
        if (sym.is(JavaStaticTerm))
          throw new FatalError(s"Assignment to static member ${sym.fullName} not supported")
        val genRhs = genExpr(rhs)
        val lhs = lhs0 match {
          case lhs: Ident => desugarIdent(lhs).getOrElse(lhs)
          case lhs => lhs
        }
        lhs match {
          case lhs: Select =>
            val qualifier = lhs.qualifier

            def ctorAssignment = (
                currentMethodSym.get.name == nme.CONSTRUCTOR &&
                currentMethodSym.get.owner == qualifier.symbol &&
                qualifier.isInstanceOf[This]
            )
            if (!sym.is(Mutable) && !ctorAssignment)
              throw new FatalError(s"Assigning to immutable field ${sym.fullName} at $pos")

            val genQual = genExpr(qualifier)

            /*if (isScalaJSDefinedJSClass(sym.owner)) {
              val genLhs = if (isExposed(sym))
                js.JSBracketSelect(genQual, js.StringLiteral(jsNameOf(sym)))
              else
                js.JSDotSelect(genQual, encodeFieldSym(sym))
              val boxedRhs =
                ensureBoxed(genRhs,
                    enteringPhase(currentRun.posterasurePhase)(rhs.tpe))
              js.Assign(genLhs, boxedRhs)
            } else {*/
              js.Assign(
                  js.Select(genQual, encodeFieldSym(sym))(toIRType(sym.info)),
                  genRhs)
            //}
          case _ =>
            js.Assign(
                js.VarRef(encodeLocalSym(sym))(toIRType(sym.info)),
                genRhs)
        }

      /** Array constructor */
      case javaSeqLiteral: JavaSeqLiteral =>
        genJavaSeqLiteral(javaSeqLiteral)

      /** A Match reaching the backend is supposed to be optimized as a switch */
      /*case mtch: Match =>
        genMatch(mtch, isStat)*/

      case tree: Closure =>
        genClosure(tree)

      /*case EmptyTree =>
        js.Skip()*/

      case _ =>
        throw new FatalError("Unexpected tree in genExpr: " +
            tree + "/" + tree.getClass + " at: " + (tree.span: Position))
    }
  } // end of genStatOrExpr()

  private def qualifierOf(fun: Tree): Tree = fun match {
    case fun: Ident =>
      fun.tpe match {
        case TermRef(prefix: TermRef, _) => tpd.ref(prefix)
        case TermRef(prefix: ThisType, _) => tpd.This(prefix.cls)
      }
    case Select(qualifier, _) =>
      qualifier
    case TypeApply(fun, _) =>
      qualifierOf(fun)
  }

  /** Gen JS this of the current class.
   *  Normally encoded straightforwardly as a JS this.
   *  But must be replaced by the `thisLocalVarIdent` local variable if there
   *  is one.
   */
  private def genThis()(implicit pos: Position): js.Tree = {
    /*if (tryingToGenMethodAsJSFunction) {
      throw new CancelGenMethodAsJSFunction(
          "Trying to generate `this` inside the body")
    }*/

    thisLocalVarIdent.fold[js.Tree] {
      js.This()(currentClassType)
    } { thisLocalIdent =>
      js.VarRef(thisLocalIdent)(currentClassType)
    }
  }

  /** Gen JS code for an Apply node (method call)
   *
   *  There's a whole bunch of varieties of Apply nodes: regular method
   *  calls, super calls, constructor calls, isInstanceOf/asInstanceOf,
   *  primitives, JS calls, etc. They are further dispatched in here.
   */
  private def genApply(tree: Apply, isStat: Boolean): js.Tree = {
    implicit val pos = tree.span
    val args = tree.args
    val sym = tree.fun.symbol

    val fun = tree.fun match {
      case fun: Ident => desugarIdent(fun)
      case fun => fun
    }

    fun match {
      case _ if isJSDefaultParam(sym) =>
        js.Transient(UndefinedParam)(toIRType(sym.info.finalResultType))

      case Select(Super(_, _), _) =>
        genSuperCall(tree, isStat)

      case Select(New(_), nme.CONSTRUCTOR) =>
        genApplyNew(tree)

      case _ =>
        if (primitives.isPrimitive(tree)) {
          genPrimitiveOp(tree, isStat)
        } else if (Erasure.Boxing.isBox(sym)) {
          // Box a primitive value (cannot be Unit)
          val arg = args.head
          makePrimitiveBox(genExpr(arg), arg.tpe)
        } else if (Erasure.Boxing.isUnbox(sym)) {
          // Unbox a primitive value (cannot be Unit)
          val arg = args.head
          makePrimitiveUnbox(genExpr(arg), tree.tpe)
        } else {
          genNormalApply(tree, isStat)
        }
    }
  }

  /** Gen JS code for a super call, of the form Class.super[mix].fun(args).
   *
   *  This does not include calls defined in mixin traits, as these are
   *  already desugared by the 'mixin' phase. Only calls to super classes
   *  remain.
   *
   *  Since a class has exactly one direct superclass, and calling a method
   *  two classes above the current one is invalid in Scala, the `mix` item is
   *  irrelevant.
   */
  private def genSuperCall(tree: Apply, isStat: Boolean): js.Tree = {
    implicit val pos = tree.span
    val Apply(fun @ Select(sup @ Super(_, mix), _), args) = tree
    val sym = fun.symbol

    if (sym == defn.Any_getClass) {
      // The only primitive that is also callable as super call
      js.GetClass(genThis())
    } else /*if (isScalaJSDefinedJSClass(currentClassSym)) {
      genJSSuperCall(tree, isStat)
    } else*/ {
      val superCall = genApplyMethodStatically(
          genThis()(sup.span), sym, genActualArgs(sym, args))

      // Initialize the module instance just after the super constructor call.
      if (isStaticModule(currentClassSym) && !isModuleInitialized &&
          currentMethodSym.get.isClassConstructor) {
        isModuleInitialized = true
        val thisType = jstpe.ClassType(encodeClassFullName(currentClassSym))
        val initModule = js.StoreModule(encodeClassRef(currentClassSym),
            js.This()(thisType))
        js.Block(superCall, initModule)
      } else {
        superCall
      }
    }
  }

  /** Gen JS code for a constructor call (new).
   *  Further refined into:
   *  * new String(...)
   *  * new of a hijacked boxed class
   *  * new of an anonymous function class that was recorded as JS function
   *  * new of a raw JS class
   *  * new Array
   *  * regular new
   */
  private def genApplyNew(tree: Apply): js.Tree = {
    implicit val pos = tree.span

    val Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) = tree
    val ctor = fun.symbol
    val tpe = tpt.tpe

    assert(ctor.isClassConstructor,
        "'new' call to non-constructor: " + ctor.name)

    val clsSym = tpe.widenDealias.typeSymbol

    if (isHijackedClass(clsSym)) {
      genNewHijackedClass(clsSym, ctor, args.map(genExpr))
    } else /*if (translatedAnonFunctions contains tpe.typeSymbol) {
      val functionMaker = translatedAnonFunctions(tpe.typeSymbol)
      functionMaker(args map genExpr)
    } else*/ if (isJSType(clsSym)) {
      if (clsSym == jsdefn.JSObjectClass && args.isEmpty) js.JSObjectConstr(Nil)
      else if (clsSym == jsdefn.JSArrayClass && args.isEmpty) js.JSArrayConstr(Nil)
      else js.JSNew(genLoadJSConstructor(clsSym), genActualJSArgs(ctor, args))
    } else {
      toTypeRef(tpe) match {
        case cls: jstpe.ClassRef =>
          js.New(cls, encodeMethodSym(ctor), genActualArgs(ctor, args))

        case other =>
          throw new FatalError(s"Non ClassRef cannot be instantiated: $other")
      }
    }
  }

  /** Gen JS code for a call to a constructor of a hijacked class.
   *  Reroute them to the `new` method with the same signature in the
   *  companion object.
   */
  private def genNewHijackedClass(clazz: Symbol, ctor: Symbol,
      args: List[js.Tree])(implicit pos: Position): js.Tree = {

    val encodedName = encodeClassFullName(clazz)
    val moduleClass = clazz.companionModule.moduleClass

    val js.Ident(initName, origName) = encodeMethodSym(ctor)
    val newMethodName = initName match {
      case "init___" =>
        "$new__" + encodedName
      case _ =>
        "$new" + initName.stripPrefix("init_") + "__" + encodedName
    }
    val newMethodIdent = js.Ident(newMethodName, origName)

    js.Apply(js.ApplyFlags.empty, genLoadModule(moduleClass), newMethodIdent, args)(
        jstpe.ClassType(encodedName))
  }

  /** Gen JS code for a primitive method call. */
  private def genPrimitiveOp(tree: Apply, isStat: Boolean): js.Tree = {
    import dotty.tools.backend.ScalaPrimitivesOps._

    implicit val pos = tree.span

    val Apply(fun, args) = tree
    val receiver = qualifierOf(fun)

    val code = primitives.getPrimitive(tree, receiver.tpe)

    if (isArithmeticOp(code) || isLogicalOp(code) || isComparisonOp(code))
      genSimpleOp(tree, receiver :: args, code)
    else if (code == CONCAT)
      genStringConcat(tree, receiver, args)
    else if (code == HASH)
      genScalaHash(tree, receiver)
    else if (isArrayOp(code))
      genArrayOp(tree, code)
    else if (code == SYNCHRONIZED)
      genSynchronized(tree, isStat)
    else if (isCoercion(code))
      genCoercion(tree, receiver, code)
    else if (code == JSPrimitives.THROW)
      genThrow(tree, args)
    else /*if (primitives.isJSPrimitive(code))
      genJSPrimitive(tree, receiver, args, code)
    else*/
      throw new FatalError(s"Unknown primitive: ${tree.symbol.fullName} at: $pos")
  }

  /** Gen JS code for a simple operation (arithmetic, logical, or comparison) */
  private def genSimpleOp(tree: Apply, args: List[Tree], code: Int): js.Tree = {
    args match {
      case List(arg)      => genSimpleUnaryOp(tree, arg, code)
      case List(lhs, rhs) => genSimpleBinaryOp(tree, lhs, rhs, code)
      case _              => throw new FatalError("Incorrect arity for primitive")
    }
  }

  /** Gen JS code for a simple unary operation. */
  private def genSimpleUnaryOp(tree: Apply, arg: Tree, code: Int): js.Tree = {
    import dotty.tools.backend.ScalaPrimitivesOps._

    implicit val pos = tree.span

    val resultIRType = toIRType(tree.tpe)
    val genArg = adaptPrimitive(genExpr(arg), resultIRType)

    (code: @switch) match {
      case POS =>
        genArg

      case NEG =>
        (resultIRType: @unchecked) match {
          case jstpe.IntType =>
            js.BinaryOp(js.BinaryOp.Int_-, js.IntLiteral(0), genArg)
          case jstpe.LongType =>
            js.BinaryOp(js.BinaryOp.Long_-, js.LongLiteral(0), genArg)
          case jstpe.FloatType =>
            js.BinaryOp(js.BinaryOp.Float_-, js.FloatLiteral(0.0f), genArg)
          case jstpe.DoubleType =>
            js.BinaryOp(js.BinaryOp.Double_-, js.DoubleLiteral(0), genArg)
        }

      case NOT =>
        (resultIRType: @unchecked) match {
          case jstpe.IntType =>
            js.BinaryOp(js.BinaryOp.Int_^, js.IntLiteral(-1), genArg)
          case jstpe.LongType =>
            js.BinaryOp(js.BinaryOp.Long_^, js.LongLiteral(-1), genArg)
        }

      case ZNOT =>
        js.UnaryOp(js.UnaryOp.Boolean_!, genArg)

      case _ =>
        throw new FatalError("Unknown unary operation code: " + code)
    }
  }

  /** Gen JS code for a simple binary operation. */
  private def genSimpleBinaryOp(tree: Apply, lhs: Tree, rhs: Tree, code: Int): js.Tree = {
    import dotty.tools.backend.ScalaPrimitivesOps._
    import js.UnaryOp._

    implicit val pos = tree.span

    val lhsIRType = toIRType(lhs.tpe)
    val rhsIRType = toIRType(rhs.tpe)

    val isShift = isShiftOp(code)

    val opType = {
      if (isShift) {
        if (lhsIRType == jstpe.LongType) jstpe.LongType
        else jstpe.IntType
      } else {
        (lhsIRType, rhsIRType) match {
          case (jstpe.DoubleType, _) | (_, jstpe.DoubleType)                          => jstpe.DoubleType
          case (jstpe.FloatType, _) | (_, jstpe.FloatType)                            => jstpe.FloatType
          case (jstpe.LongType, _) | (_, jstpe.LongType)                              => jstpe.LongType
          case (jstpe.IntType | jstpe.ByteType | jstpe.ShortType | jstpe.CharType, _) => jstpe.IntType
          case (_, jstpe.IntType | jstpe.ByteType | jstpe.ShortType | jstpe.CharType) => jstpe.IntType
          case (jstpe.BooleanType, _) | (_, jstpe.BooleanType)                        => jstpe.BooleanType
          case _                                                                      => jstpe.AnyType
        }
      }
    }

    val lsrc =
      if (opType == jstpe.AnyType) genExpr(lhs)
      else adaptPrimitive(genExpr(lhs), opType)
    val rsrc =
      if (opType == jstpe.AnyType) genExpr(rhs)
      else adaptPrimitive(genExpr(rhs), if (isShift) jstpe.IntType else opType)

    if (opType == jstpe.AnyType && isUniversalEqualityOp(code)) {
      genUniversalEqualityOp(lhs.tpe, rhs.tpe, lsrc, rsrc, code)
    } else if (code == ZOR) {
      js.If(lsrc, js.BooleanLiteral(true), rsrc)(jstpe.BooleanType)
    } else if (code == ZAND) {
      js.If(lsrc, rsrc, js.BooleanLiteral(false))(jstpe.BooleanType)
    } else {
      import js.BinaryOp._

      (opType: @unchecked) match {
        case jstpe.IntType =>
          val op = (code: @switch) match {
            case ADD => Int_+
            case SUB => Int_-
            case MUL => Int_*
            case DIV => Int_/
            case MOD => Int_%
            case OR  => Int_|
            case AND => Int_&
            case XOR => Int_^
            case LSL => Int_<<
            case LSR => Int_>>>
            case ASR => Int_>>

            case EQ => Int_==
            case NE => Int_!=
            case LT => Int_<
            case LE => Int_<=
            case GT => Int_>
            case GE => Int_>=
          }
          js.BinaryOp(op, lsrc, rsrc)

        case jstpe.FloatType =>
          def withFloats(op: Int): js.Tree =
            js.BinaryOp(op, lsrc, rsrc)

          def toDouble(value: js.Tree): js.Tree =
            js.UnaryOp(js.UnaryOp.FloatToDouble, value)

          def withDoubles(op: Int): js.Tree =
            js.BinaryOp(op, toDouble(lsrc), toDouble(rsrc))

          (code: @switch) match {
            case ADD => withFloats(Float_+)
            case SUB => withFloats(Float_-)
            case MUL => withFloats(Float_*)
            case DIV => withFloats(Float_/)
            case MOD => withFloats(Float_%)

            case EQ => withDoubles(Double_==)
            case NE => withDoubles(Double_!=)
            case LT => withDoubles(Double_<)
            case LE => withDoubles(Double_<=)
            case GT => withDoubles(Double_>)
            case GE => withDoubles(Double_>=)
          }

        case jstpe.DoubleType =>
          val op = (code: @switch) match {
            case ADD => Double_+
            case SUB => Double_-
            case MUL => Double_*
            case DIV => Double_/
            case MOD => Double_%

            case EQ => Double_==
            case NE => Double_!=
            case LT => Double_<
            case LE => Double_<=
            case GT => Double_>
            case GE => Double_>=
          }
          js.BinaryOp(op, lsrc, rsrc)

        case jstpe.LongType =>
          val op = (code: @switch) match {
            case ADD => Long_+
            case SUB => Long_-
            case MUL => Long_*
            case DIV => Long_/
            case MOD => Long_%
            case OR  => Long_|
            case XOR => Long_^
            case AND => Long_&
            case LSL => Long_<<
            case LSR => Long_>>>
            case ASR => Long_>>

            case EQ => Long_==
            case NE => Long_!=
            case LT => Long_<
            case LE => Long_<=
            case GT => Long_>
            case GE => Long_>=
          }
          js.BinaryOp(op, lsrc, rsrc)

        case jstpe.BooleanType =>
          val op = (code: @switch) match {
            case EQ  => Boolean_==
            case NE  => Boolean_!=
            case OR  => Boolean_|
            case AND => Boolean_&
            case XOR => Boolean_!=
          }
          js.BinaryOp(op, lsrc, rsrc)

        case jstpe.AnyType =>
          val op = code match {
            case ID => ===
            case NI => !==
          }
          js.BinaryOp(op, lsrc, rsrc)
      }
    }
  }

  private def adaptPrimitive(value: js.Tree, to: jstpe.Type)(
      implicit pos: Position): js.Tree = {
    genConversion(value.tpe, to, value)
  }

  /* This method corresponds to the method of the same name in
   * BCodeBodyBuilder of the JVM back-end. It ends up calling the method
   * BCodeIdiomatic.emitT2T, whose logic we replicate here.
    */
  private def genConversion(from: jstpe.Type, to: jstpe.Type, value: js.Tree)(
      implicit pos: Position): js.Tree = {
    import js.UnaryOp._

    if (from == to || from == jstpe.NothingType) {
      value
    } else if (from == jstpe.BooleanType || to == jstpe.BooleanType) {
      throw new AssertionError(s"Invalid genConversion from $from to $to")
    } else {
      def intValue = (from: @unchecked) match {
        case jstpe.IntType    => value
        case jstpe.CharType   => js.UnaryOp(CharToInt, value)
        case jstpe.ByteType   => js.UnaryOp(ByteToInt, value)
        case jstpe.ShortType  => js.UnaryOp(ShortToInt, value)
        case jstpe.LongType   => js.UnaryOp(LongToInt, value)
        case jstpe.FloatType  => js.UnaryOp(DoubleToInt, js.UnaryOp(FloatToDouble, value))
        case jstpe.DoubleType => js.UnaryOp(DoubleToInt, value)
      }

      def doubleValue = from match {
        case jstpe.DoubleType => value
        case jstpe.FloatType  => js.UnaryOp(FloatToDouble, value)
        case jstpe.LongType   => js.UnaryOp(LongToDouble, value)
        case _                => js.UnaryOp(IntToDouble, intValue)
      }

      (to: @unchecked) match {
        case jstpe.CharType =>
          js.UnaryOp(IntToChar, intValue)
        case jstpe.ByteType =>
          js.UnaryOp(IntToByte, intValue)
        case jstpe.ShortType =>
          js.UnaryOp(IntToShort, intValue)
        case jstpe.IntType =>
          intValue
        case jstpe.LongType =>
          from match {
            case jstpe.FloatType | jstpe.DoubleType =>
              js.UnaryOp(DoubleToLong, doubleValue)
            case _ =>
              js.UnaryOp(IntToLong, intValue)
          }
        case jstpe.FloatType =>
          js.UnaryOp(js.UnaryOp.DoubleToFloat, doubleValue)
        case jstpe.DoubleType =>
          doubleValue
      }
    }
  }

  /** Gen JS code for a universal equality test. */
  private def genUniversalEqualityOp(ltpe: Type, rtpe: Type, lhs: js.Tree, rhs: js.Tree, code: Int)(
      implicit pos: Position): js.Tree = {

    import dotty.tools.backend.ScalaPrimitivesOps._

    val bypassEqEq = {
      // Do not call equals if we have a literal null at either side.
      lhs.isInstanceOf[js.Null] ||
      rhs.isInstanceOf[js.Null]
    }

    if (bypassEqEq) {
      js.BinaryOp(
          if (code == EQ) js.BinaryOp.=== else js.BinaryOp.!==,
          lhs, rhs)
    } else {
      val body = genEqEqPrimitive(ltpe, rtpe, lhs, rhs)
      if (code == EQ) body
      else js.UnaryOp(js.UnaryOp.Boolean_!, body)
    }
  }

  private lazy val externalEqualsNumNum: Symbol =
    defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumNum)
  private lazy val externalEqualsNumChar: Symbol =
    NoSymbol // ctx.requiredMethod(BoxesRunTimeTypeRef, nme.equalsNumChar) // this method is private
  private lazy val externalEqualsNumObject: Symbol =
    defn.BoxesRunTimeModule.requiredMethod(nme.equalsNumObject)
  private lazy val externalEquals: Symbol =
    defn.BoxesRunTimeClass.info.decl(nme.equals_).suchThat(toDenot(_).info.firstParamTypes.size == 2).symbol

  /** Gen JS code for a call to Any.== */
  private def genEqEqPrimitive(ltpe: Type, rtpe: Type, lsrc: js.Tree, rsrc: js.Tree)(
      implicit pos: Position): js.Tree = {
    ctx.debuglog(s"$ltpe == $rtpe")
    val lsym = ltpe.widenDealias.typeSymbol.asClass
    val rsym = rtpe.widenDealias.typeSymbol.asClass

    /* True if the equality comparison is between values that require the
     * use of the rich equality comparator
     * (scala.runtime.BoxesRunTime.equals).
     * This is the case when either side of the comparison might have a
     * run-time type subtype of java.lang.Number or java.lang.Character,
     * **which includes when either is a JS type**.
     * When it is statically known that both sides are equal and subtypes of
     * Number or Character, not using the rich equality is possible (their
     * own equals method will do ok.)
     */
    val mustUseAnyComparator: Boolean = {
      isJSType(lsym) || isJSType(rsym) || {
        val p = ctx.platform
        val areSameFinals = lsym.is(Final) && rsym.is(Final) && (ltpe =:= rtpe)
        !areSameFinals && p.isMaybeBoxed(lsym) && p.isMaybeBoxed(rsym)
      }
    }

    if (mustUseAnyComparator) {
      val equalsMethod: Symbol = {
        // scalastyle:off line.size.limit
        val ptfm = ctx.platform
        if (lsym.derivesFrom(defn.BoxedNumberClass)) {
          if (rsym.derivesFrom(defn.BoxedNumberClass)) externalEqualsNumNum
          else if (rsym.derivesFrom(defn.BoxedCharClass)) externalEqualsNumObject // will be externalEqualsNumChar in 2.12, SI-9030
          else externalEqualsNumObject
        } else externalEquals
        // scalastyle:on line.size.limit
      }
      genModuleApplyMethod(equalsMethod, List(lsrc, rsrc))
    } else {
      // if (lsrc eq null) rsrc eq null else lsrc.equals(rsrc)
      if (lsym == defn.StringClass) {
        // String.equals(that) === (this eq that)
        js.BinaryOp(js.BinaryOp.===, lsrc, rsrc)
      } else {
        /* This requires to evaluate both operands in local values first.
         * The optimizer will eliminate them if possible.
         */
        val ltemp = js.VarDef(freshLocalIdent(), lsrc.tpe, mutable = false, lsrc)
        val rtemp = js.VarDef(freshLocalIdent(), rsrc.tpe, mutable = false, rsrc)
        js.Block(
            ltemp,
            rtemp,
            js.If(js.BinaryOp(js.BinaryOp.===, ltemp.ref, js.Null()),
                js.BinaryOp(js.BinaryOp.===, rtemp.ref, js.Null()),
                genApplyMethod(ltemp.ref, defn.Any_equals, List(rtemp.ref)))(
                jstpe.BooleanType))
      }
    }
  }

  /** Gen JS code for string concatenation.
   */
  private def genStringConcat(tree: Apply, receiver: Tree,
      args: List[Tree]): js.Tree = {
    implicit val pos = tree.span

    val arg = args.head

    /* Primitive number types such as scala.Int have a
     *   def +(s: String): String
     * method, which is why we have to box the lhs sometimes.
     * Otherwise, both lhs and rhs are already reference types (Any or String)
     * so boxing is not necessary (in particular, rhs is never a primitive).
     */
    assert(!isPrimitiveValueType(receiver.tpe) || arg.tpe.isRef(defn.StringClass))
    assert(!isPrimitiveValueType(arg.tpe))

    val genLhs = {
      val genLhs0 = genExpr(receiver)
      // Box the receiver if it is a primitive value
      if (!isPrimitiveValueType(receiver.tpe)) genLhs0
      else makePrimitiveBox(genLhs0, receiver.tpe)
    }

    val genRhs = genExpr(arg)

    js.BinaryOp(js.BinaryOp.String_+, genLhs, genRhs)
  }

  /** Gen JS code for a call to Any.## */
  private def genScalaHash(tree: Apply, receiver: Tree): js.Tree = {
    implicit val pos = tree.span

    genModuleApplyMethod(defn.ScalaRuntimeModule.requiredMethod(nme.hash_),
        List(genExpr(receiver)))
  }

  /** Gen JS code for an array operation (get, set or length) */
  private def genArrayOp(tree: Tree, code: Int): js.Tree = {
    import dotty.tools.backend.ScalaPrimitivesOps._

    implicit val pos = tree.span

    val Apply(fun, args) = tree
    val arrayObj = qualifierOf(fun)

    val genArray = genExpr(arrayObj)
    val genArgs = args.map(genExpr)

    def elementType: Type = arrayObj.tpe.widenDealias match {
      case defn.ArrayOf(el)  => el
      case JavaArrayType(el) => el
      case tpe =>
        val msg = ex"expected Array $tpe"
        ctx.error(msg)
        ErrorType(msg)
    }

    def genSelect(): js.Tree =
      js.ArraySelect(genArray, genArgs(0))(toIRType(elementType))

    if (isArrayGet(code)) {
      // get an item of the array
      assert(args.length == 1,
          s"Array get requires 1 argument, found ${args.length} in $tree")
      genSelect()
    } else if (isArraySet(code)) {
      // set an item of the array
      assert(args.length == 2,
          s"Array set requires 2 arguments, found ${args.length} in $tree")
      js.Assign(genSelect(), genArgs(1))
    } else {
      // length of the array
      js.ArrayLength(genArray)
    }
  }

  /** Gen JS code for a call to AnyRef.synchronized */
  private def genSynchronized(tree: Apply, isStat: Boolean): js.Tree = {
    /* JavaScript is single-threaded, so we can drop the
     * synchronization altogether.
     */
    val Apply(fun, List(arg)) = tree
    val receiver = qualifierOf(fun)

    val genReceiver = genExpr(receiver)
    val genArg = genStatOrExpr(arg, isStat)

    genReceiver match {
      case js.This() =>
        // common case for which there is no side-effect nor NPE
        genArg
      case _ =>
        implicit val pos = tree.span
        /* TODO Check for a null receiver?
         * In theory, it's UB, but that decision should be left for link time.
         */
        js.Block(genReceiver, genArg)
    }
  }

  /** Gen JS code for a coercion */
  private def genCoercion(tree: Apply, receiver: Tree, code: Int): js.Tree = {
    implicit val pos = tree.span

    val source = genExpr(receiver)
    val resultType = toIRType(tree.tpe)
    adaptPrimitive(source, resultType)
  }

  /** Gen a call to the special `throw` method. */
  private def genThrow(tree: Apply, args: List[Tree]): js.Tree = {
    implicit val pos = tree.span
    val exception = args.head
    val genException = genExpr(exception)
    js.Throw {
      if (exception.tpe.widenDealias.typeSymbol.derivesFrom(jsdefn.JavaScriptExceptionClass)) {
        genModuleApplyMethod(
            jsdefn.Runtime_unwrapJavaScriptException,
            List(genException))
      } else {
        genException
      }
    }
  }

  /** Gen a "normal" apply (to a true method).
   *
   *  But even these are further refined into:
   *  * Methods of java.lang.String, which are redirected to the
   *    RuntimeString trait implementation.
   *  * Calls to methods of raw JS types (Scala.js -> JS interop)
   *  * Calls to methods in impl classes of Scala2 traits.
   *  * Regular method call
   */
  private def genNormalApply(tree: Apply, isStat: Boolean): js.Tree = {
    implicit val pos = tree.span

    val fun = tree.fun match {
      case fun: Ident => desugarIdent(fun).get
      case fun: Select => fun
    }
    val receiver = fun.qualifier
    val args = tree.args
    val sym = fun.symbol

    def isStringMethodFromObject: Boolean = sym.name match {
      case nme.toString_ | nme.equals_ | nme.hashCode_ => true
      case _                                           => false
    }

    if (isJSType(sym.owner)) {
      //if (!isScalaJSDefinedJSClass(sym.owner) || isExposed(sym))
        genApplyJSMethodGeneric(tree, sym, genExpr(receiver), genActualJSArgs(sym, args), isStat)
      /*else
        genApplyJSClassMethod(genExpr(receiver), sym, genActualArgs(sym, args))*/
    } else {
      genApplyMethodMaybeStatically(genExpr(receiver), sym, genActualArgs(sym, args))
    }
  }

  /** Gen JS code for a call to a JS method (of a subclass of `js.Any`).
   *
   *  Basically it boils down to calling the method as a `JSBracketSelect`,
   *  without name mangling. But other aspects come into play:
   *
   *  - Operator methods are translated to JS operators (not method calls)
   *  - `apply` is translated as a function call, i.e., `o()` instead of `o.apply()`
   *  - Scala varargs are turned into JS varargs (see `genPrimitiveJSArgs()`)
   *  - Getters and parameterless methods are translated as `JSBracketSelect`
   *  - Setters are translated to `Assign` to `JSBracketSelect`
   */
  private def genApplyJSMethodGeneric(tree: Tree, sym: Symbol,
      receiver: js.Tree, args: List[js.TreeOrJSSpread], isStat: Boolean,
      jsSuperClassValue: Option[js.Tree] = None)(
      implicit pos: Position): js.Tree = {

    implicit val pos = tree.span

    def noSpread = !args.exists(_.isInstanceOf[js.JSSpread])
    val argc = args.size // meaningful only for methods that don't have varargs

    def requireNotSuper(): Unit = {
      if (jsSuperClassValue.isDefined)
        ctx.error("Illegal super call in Scala.js-defined JS class", tree.sourcePos)
    }

    def requireNotSpread(arg: js.TreeOrJSSpread): js.Tree =
      arg.asInstanceOf[js.Tree]

    def hasExplicitJSEncoding = {
      sym.hasAnnotation(jsdefn.JSNameAnnot) ||
      sym.hasAnnotation(jsdefn.JSBracketAccessAnnot) ||
      sym.hasAnnotation(jsdefn.JSBracketCallAnnot)
    }

    val boxedResult = sym.name match {
      case JSUnaryOpMethodName(code) if argc == 0 =>
        requireNotSuper()
        js.JSUnaryOp(code, receiver)

      case JSBinaryOpMethodName(code) if argc == 1 =>
        requireNotSuper()
        js.JSBinaryOp(code, receiver, requireNotSpread(args.head))

      case nme.apply if !hasExplicitJSEncoding =>
        requireNotSuper()
        if (jsdefn.isJSThisFunctionClass(sym.owner))
          js.JSBracketMethodApply(receiver, js.StringLiteral("call"), args)
        else
          js.JSFunctionApply(receiver, args)

      case _ =>
        def jsFunName = js.StringLiteral(jsNameOf(sym))

        def genSuperReference(propName: js.Tree): js.Tree = {
          jsSuperClassValue.fold[js.Tree] {
            js.JSBracketSelect(receiver, propName)
          } { superClassValue =>
            js.JSSuperBracketSelect(superClassValue, receiver, propName)
          }
        }

        def genSelectGet(propName: js.Tree): js.Tree =
          genSuperReference(propName)

        def genSelectSet(propName: js.Tree, value: js.Tree): js.Tree =
          js.Assign(genSuperReference(propName), value)

        def genCall(methodName: js.Tree, args: List[js.TreeOrJSSpread]): js.Tree = {
          jsSuperClassValue.fold[js.Tree] {
            js.JSBracketMethodApply(
                receiver, methodName, args)
          } { superClassValue =>
            js.JSSuperBracketCall(superClassValue, receiver, methodName, args)
          }
        }

        if (isJSGetter(sym)) {
          assert(noSpread && argc == 0)
          genSelectGet(jsFunName)
        } else if (isJSSetter(sym)) {
          assert(noSpread && argc == 1)
          genSelectSet(jsFunName, requireNotSpread(args.head))
        } else if (isJSBracketAccess(sym)) {
          assert(noSpread && (argc == 1 || argc == 2),
              s"@JSBracketAccess methods should have 1 or 2 non-varargs arguments")
          (args: @unchecked) match {
            case List(keyArg) =>
              genSelectGet(requireNotSpread(keyArg))
            case List(keyArg, valueArg) =>
              genSelectSet(requireNotSpread(keyArg), requireNotSpread(valueArg))
          }
        } else if (isJSBracketCall(sym)) {
          val (methodName, actualArgs) = extractFirstArg(args)
          genCall(methodName, actualArgs)
        } else {
          genCall(jsFunName, args)
        }
    }

    if (isStat) {
      boxedResult
    } else {
      val tpe = ctx.atPhase(ctx.elimErasedValueTypePhase) { implicit ctx =>
        sym.info.finalResultType
      }
      unbox(boxedResult, tpe)
    }
  }

  private object JSUnaryOpMethodName {
    private val map = Map(
      nme.UNARY_+ -> js.JSUnaryOp.+,
      nme.UNARY_- -> js.JSUnaryOp.-,
      nme.UNARY_~ -> js.JSUnaryOp.~,
      nme.UNARY_! -> js.JSUnaryOp.!
    )

    def unapply(name: TermName): Option[js.JSUnaryOp.Code] =
      map.get(name)
  }

  private object JSBinaryOpMethodName {
    private val map = Map(
      nme.ADD -> js.JSBinaryOp.+,
      nme.SUB -> js.JSBinaryOp.-,
      nme.MUL -> js.JSBinaryOp.*,
      nme.DIV -> js.JSBinaryOp./,
      nme.MOD -> js.JSBinaryOp.%,

      nme.LSL -> js.JSBinaryOp.<<,
      nme.ASR -> js.JSBinaryOp.>>,
      nme.LSR -> js.JSBinaryOp.>>>,
      nme.OR  -> js.JSBinaryOp.|,
      nme.AND -> js.JSBinaryOp.&,
      nme.XOR -> js.JSBinaryOp.^,

      nme.LT -> js.JSBinaryOp.<,
      nme.LE -> js.JSBinaryOp.<=,
      nme.GT -> js.JSBinaryOp.>,
      nme.GE -> js.JSBinaryOp.>=,

      nme.ZAND -> js.JSBinaryOp.&&,
      nme.ZOR  -> js.JSBinaryOp.||
    )

    def unapply(name: TermName): Option[js.JSBinaryOp.Code] =
      map.get(name)
  }

  /** Extract the first argument in a list of actual arguments.
   *
   *  This is nothing else than decomposing into head and tail, except that
   *  we assert that the first element is not a JSSpread.
   */
  private def extractFirstArg(args: List[js.TreeOrJSSpread]): (js.Tree, List[js.TreeOrJSSpread]) = {
    assert(args.nonEmpty,
        "Trying to extract the first argument of an empty argument list")
    val firstArg = args.head
    assert(!firstArg.isInstanceOf[js.JSSpread],
        "Trying to extract the first argument of an argument list starting " +
        "with a Spread argument: " + firstArg)
    (firstArg.asInstanceOf[js.Tree], args.tail)
  }

  /** Gen JS code for a call to a polymorphic method.
   *
   *  The only methods that reach the back-end as polymorphic are
   *  `isInstanceOf` and `asInstanceOf`.
   *
   *  (Well, in fact `DottyRunTime.newRefArray` too, but it is handled as a
   *  primitive instead.)
   */
  private def genTypeApply(tree: TypeApply): js.Tree = {
    implicit val pos = tree.span

    val TypeApply(fun, targs) = tree

    val sym = fun.symbol
    val receiver = qualifierOf(fun)

    val to = targs.head.tpe

    assert(!isPrimitiveValueType(receiver.tpe),
        s"Found receiver of type test with primitive type ${receiver.tpe} at $pos")
    assert(!isPrimitiveValueType(to),
        s"Found target type of type test with primitive type ${receiver.tpe} at $pos")

    val genReceiver = genExpr(receiver)

    if (sym == defn.Any_asInstanceOf) {
      genAsInstanceOf(genReceiver, to)
    } else if (sym == defn.Any_isInstanceOf) {
      genIsInstanceOf(tree, genReceiver, to)
    } else {
      throw new FatalError(
          s"Unexpected type application $fun with symbol ${sym.fullName}")
    }
  }

  /** Gen JS code for a Java Seq literal. */
  private def genJavaSeqLiteral(tree: JavaSeqLiteral): js.Tree = {
    implicit val pos = tree.span

    val genElems = tree.elems.map(genExpr)
    val arrayTypeRef = toTypeRef(tree.tpe).asInstanceOf[jstpe.ArrayTypeRef]
    js.ArrayValue(arrayTypeRef, genElems)
  }

  /** Gen JS code for a closure.
   *
   *  Input: a `Closure` tree of the form
   *  {{{
   *  Closure(env, call, functionalInterface)
   *  }}}
   *  representing the pseudo-syntax
   *  {{{
   *  { (p1, ..., pm) => call(env1, ..., envn, p1, ..., pm) }: functionInterface
   *  }}}
   *  where `envi` are identifiers in the local scope. The qualifier of `call`
   *  is also implicitly captured.
   *
   *  Output: a `js.Closure` tree of the form
   *  {{{
   *  js.Closure(formalCaptures, formalParams, body, actualCaptures)
   *  }}}
   *  representing the pseudo-syntax
   *  {{{
   *  lambda<formalCapture1 = actualCapture1, ..., formalCaptureN = actualCaptureN>(
   *      formalParam1, ..., formalParamM) = body
   *  }}}
   *  where the `actualCaptures` and `body` are, in general, arbitrary
   *  expressions. But in this case, `actualCaptures` will be identifiers from
   *  `env`, and the `body` will be of the form
   *  {{{
   *  call(formalCapture1.ref, ..., formalCaptureN.ref,
   *      formalParam1.ref, ...formalParamM.ref)
   *  }}}
   *
   *  When the `js.Closure` node is evaluated, i.e., when the closure value is
   *  created, the expressions of the `actualCaptures` are evaluated, and the
   *  results of those evaluations is "stored" in the environment of the
   *  closure as the corresponding `formalCapture`.
   *
   *  When we later *call* the closure, the `formalCaptures` already have their
   *  values from the environment, and they are available in the `body`. The
   *  `formalParams` of the created closure receive their values from the
   *  actual arguments at the call-site of the closure, and they are also
   *  available in the `body`.
   */
  private def genClosure(tree: Closure): js.Tree = {
    implicit val pos = tree.span
    val Closure(env, call, functionalInterface) = tree

    val envSize = env.size

    val (fun, args) = call match {
      // case Apply(fun, args) => (fun, args) // Conjectured not to happen
      case t @ Select(_, _) => (t, Nil)
      case t @ Ident(_) => (t, Nil)
    }
    val sym = fun.symbol

    val qualifier = qualifierOf(fun)
    val allCaptureValues = qualifier :: env

    val formalAndActualCaptures = allCaptureValues.map { value =>
      implicit val pos = value.span
      val formalIdent = value match {
        case Ident(name) => freshLocalIdent(name.toString)
        case This(_)     => freshLocalIdent("this")
        case _           => freshLocalIdent()
      }
      val formalCapture =
        js.ParamDef(formalIdent, toIRType(value.tpe), mutable = false, rest = false)
      val actualCapture = genExpr(value)
      (formalCapture, actualCapture)
    }
    val (formalCaptures, actualCaptures) = formalAndActualCaptures.unzip

    val formalParamNames = sym.info.paramNamess.flatten.drop(envSize)
    val formalParamTypes = sym.info.paramInfoss.flatten.drop(envSize)
    val formalParamNamesAndTypes = formalParamNames.zip(formalParamTypes)
    val formalAndActualParams = formalParamNamesAndTypes.map {
      case (name, tpe) =>
        val formalParam = js.ParamDef(freshLocalIdent(name.toString),
            jstpe.AnyType, mutable = false, rest = false)
        val actualParam = unbox(formalParam.ref, tpe)
        (formalParam, actualParam)
    }
    val (formalParams, actualParams) = formalAndActualParams.unzip

    val genBody = {
      val thisCaptureRef :: argCaptureRefs = formalCaptures.map(_.ref)
      val call = genApplyMethodMaybeStatically(thisCaptureRef, sym,
          argCaptureRefs ::: actualParams)
      box(call, sym.info.finalResultType)
    }

    val closure = js.Closure(arrow = true, formalCaptures, formalParams, genBody, actualCaptures)
    ctx.debuglog(closure.toString)

    val funInterfaceSym = functionalInterface.tpe.widenDealias.typeSymbol
    if (jsdefn.isJSFunctionClass(funInterfaceSym)) {
      closure
    } else {
      assert(!funInterfaceSym.exists || defn.isFunctionClass(funInterfaceSym),
          s"Invalid functional interface $funInterfaceSym reached the back-end")
      val cls = "sjsr_AnonFunction" + formalParams.size
      val ctor = js.Ident("init___sjs_js_Function" + formalParams.size)
      js.New(jstpe.ClassRef(cls), ctor, List(closure))
    }
  }

  /** Boxes a value of the given type before `elimErasedValueType`.
   *
   *  This should be used when sending values to a JavaScript context, which
   *  is erased/boxed at the IR level, although it is not erased at the
   *  dotty/JVM level.
   *
   *  @param expr Tree to be boxed if needed.
   *  @param tpeEnteringElimErasedValueType The type of `expr` as it was
   *    entering the `elimErasedValueType` phase.
   */
  private def box(expr: js.Tree, tpeEnteringElimErasedValueType: Type)(
      implicit pos: Position): js.Tree = {

    tpeEnteringElimErasedValueType match {
      case tpe if isPrimitiveValueType(tpe) =>
        makePrimitiveBox(expr, tpe)

      /*case tpe: ErasedValueType =>
        val boxedClass = tpe.valueClazz
        val ctor = boxedClass.primaryConstructor
        genNew(boxedClass, ctor, List(expr))*/

      case _ =>
        expr
    }
  }

  /** Unboxes a value typed as Any to the given type before `elimErasedValueType`.
   *
   *  This should be used when receiving values from a JavaScript context,
   *  which is erased/boxed at the IR level, although it is not erased at the
   *  dotty/JVM level.
   *
   *  @param expr Tree to be extracted.
   *  @param tpeEnteringElimErasedValueType The type of `expr` as it was
   *    entering the `elimErasedValueType` phase.
   */
  private def unbox(expr: js.Tree, tpeEnteringElimErasedValueType: Type)(
      implicit pos: Position): js.Tree = {

    tpeEnteringElimErasedValueType match {
      case tpe if isPrimitiveValueType(tpe) =>
        makePrimitiveUnbox(expr, tpe)

      /*case tpe: ErasedValueType =>
        val boxedClass = tpe.valueClazz
        val unboxMethod = boxedClass.derivedValueClassUnbox
        val content = genApplyMethod(
            genAsInstanceOf(expr, tpe), unboxMethod, Nil)
        if (unboxMethod.tpe.resultType <:< tpe.erasedUnderlying)
          content
        else
          fromAny(content, tpe.erasedUnderlying)*/

      case tpe =>
        genAsInstanceOf(expr, tpe)
    }
  }

  /** Gen JS code for an asInstanceOf cast (for reference types only) */
  private def genAsInstanceOf(value: js.Tree, to: Type)(
      implicit pos: Position): js.Tree = {

    val sym = to.widenDealias.typeSymbol

    if (sym == defn.ObjectClass || isJSType(sym)) {
      /* asInstanceOf[Object] always succeeds, and
       * asInstanceOf to a raw JS type is completely erased.
       */
      value
    } else {
      js.AsInstanceOf(value, toTypeRef(to))
    }
  }

  /** Gen JS code for an isInstanceOf test (for reference types only) */
  private def genIsInstanceOf(tree: Tree, value: js.Tree, to: Type): js.Tree = {
    implicit val pos = tree.span
    val sym = to.widenDealias.typeSymbol

    if (sym == defn.ObjectClass) {
      js.BinaryOp(js.BinaryOp.!==, value, js.Null())
    } else if (isJSType(sym)) {
      if (sym.is(Trait)) {
        ctx.error(
            s"isInstanceOf[${sym.fullName}] not supported because it is a JS trait",
            tree.sourcePos)
        js.BooleanLiteral(true)
      } else {
        js.Unbox(js.JSBinaryOp(
            js.JSBinaryOp.instanceof, value, genLoadJSConstructor(sym)), 'Z')
      }
    } else {
      js.IsInstanceOf(value, toTypeRef(to))
    }
  }

  /** Gen a statically linked call to an instance method. */
  private def genApplyMethodMaybeStatically(receiver: js.Tree, method: Symbol,
      arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
    if (method.isPrivate || method.isClassConstructor)
      genApplyMethodStatically(receiver, method, arguments)
    else
      genApplyMethod(receiver, method, arguments)
  }

  /** Gen a dynamically linked call to a Scala method. */
  private def genApplyMethod(receiver: js.Tree, method: Symbol,
      arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    assert(!method.isPrivate,
        s"Cannot generate a dynamic call to private method $method at $pos")
    js.Apply(js.ApplyFlags.empty, receiver, encodeMethodSym(method), arguments)(
        toIRType(patchedResultType(method)))
  }

  /** Gen a statically linked call to an instance method. */
  private def genApplyMethodStatically(receiver: js.Tree, method: Symbol,
      arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
    val flags = js.ApplyFlags.empty
      .withPrivate(method.isPrivate && !method.isClassConstructor)
      .withConstructor(method.isClassConstructor)
    js.ApplyStatically(flags, receiver, encodeClassRef(method.owner),
        encodeMethodSym(method), arguments)(
        toIRType(patchedResultType(method)))
  }

  /** Gen a call to a static method. */
  private def genApplyStatic(method: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    js.ApplyStatic(js.ApplyFlags.empty, encodeClassRef(method.owner),
        encodeMethodSym(method), arguments)(
        toIRType(patchedResultType(method)))
  }

  /** Gen a call to a non-exposed method of a non-native JS class. */
  private def genApplyJSClassMethod(receiver: js.Tree, method: Symbol,
      arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
    genApplyStatic(method, receiver :: arguments)
  }

  /** Gen a call to a method of a Scala top-level module. */
  private def genModuleApplyMethod(methodSym: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    genApplyMethod(genLoadModule(methodSym.owner), methodSym, arguments)
  }

  /** Gen a boxing operation (tpe is the primitive type) */
  private def makePrimitiveBox(expr: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    toTypeRef(tpe) match {
      case jstpe.ClassRef(ir.Definitions.VoidClass) =>
        js.Block(expr, js.Undefined())
      case jstpe.ClassRef(cls) if ir.Definitions.PrimitiveClasses.contains(cls) =>
        expr // box is identity for all non-Unit types
      case typeRef =>
        throw new FatalError(
            s"makePrimitiveBox requires a primitive type, found $typeRef for $tpe at $pos")
    }
  }

  /** Gen an unboxing operation (tpe is the primitive type) */
  private def makePrimitiveUnbox(expr: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    toTypeRef(tpe) match {
      case jstpe.ClassRef(cls) if ir.Definitions.PrimitiveClasses.contains(cls) =>
        assert(cls.length == 1)
        cls.charAt(0) match {
          case 'V' =>
            expr
          case primitiveCharCode =>
            js.Unbox(expr, primitiveCharCode)
        }

      case _ =>
        throw new FatalError(
            s"makePrimitiveUnbox requires a primitive type, found $tpe at $pos")
    }
  }

  /** Gen actual actual arguments to Scala method call.
   *  Returns a list of the transformed arguments.
   *
   *  This tries to optimize repeated arguments (varargs) by turning them
   *  into js.WrappedArray instead of Scala wrapped arrays.
   */
  private def genActualArgs(sym: Symbol, args: List[Tree])(
      implicit pos: Position): List[js.Tree] = {
    args.map(genExpr)
    /*val wereRepeated = exitingPhase(currentRun.typerPhase) {
      sym.tpe.params.map(p => isScalaRepeatedParamType(p.tpe))
    }

    if (wereRepeated.size > args.size) {
      // Should not happen, but let's not crash
      args.map(genExpr)
    } else {
      /* Arguments that are in excess compared to the type signature after
       * erasure are lambda-lifted arguments. They cannot be repeated, hence
       * the extension to `false`.
       */
      for ((arg, wasRepeated) <- args.zipAll(wereRepeated, EmptyTree, false)) yield {
        if (wasRepeated) {
          tryGenRepeatedParamAsJSArray(arg, handleNil = false).fold {
            genExpr(arg)
          } { genArgs =>
            genNew(WrappedArrayClass, WrappedArray_ctor,
                List(js.JSArrayConstr(genArgs)))
          }
        } else {
          genExpr(arg)
        }
      }
    }*/
  }

  /** Gen actual actual arguments to a JS method call.
   *  Returns a list of the transformed arguments.
   *
   *  - TODO Repeated arguments (varargs) are expanded
   *  - Default arguments are omitted or replaced by undefined
   *  - All arguments are boxed
   *
   *  Repeated arguments that cannot be expanded at compile time (i.e., if a
   *  Seq is passed to a varargs parameter with the syntax `seq: _*`) will be
   *  wrapped in a [[js.JSSpread]] node to be expanded at runtime.
   */
  private def genActualJSArgs(sym: Symbol, args: List[Tree])(
      implicit pos: Position): List[js.TreeOrJSSpread] = {

    def paramNamesAndTypes(implicit ctx: Context): List[(Names.TermName, Type)] =
      sym.info.paramNamess.flatten.zip(sym.info.paramInfoss.flatten)

    val wereRepeated = ctx.atPhase(ctx.elimRepeatedPhase) { implicit ctx =>
      val list = for ((name, tpe) <- paramNamesAndTypes)
        yield (name -> tpe.isRepeatedParam)
      list.toMap
    }

    val paramTypes = ctx.atPhase(ctx.elimErasedValueTypePhase) { implicit ctx =>
      paramNamesAndTypes.toMap
    }

    var reversedArgs: List[js.TreeOrJSSpread] = Nil

    val argsParamNamesAndTypes = args.zip(paramNamesAndTypes)
    for ((arg, (paramName, paramType)) <- argsParamNamesAndTypes) {
      val wasRepeated = wereRepeated.getOrElse(paramName, false)
      if (wasRepeated) {
        reversedArgs =
          genJSRepeatedParam(arg) reverse_::: reversedArgs
      } else {
        val unboxedArg = genExpr(arg)
        val boxedArg = unboxedArg match {
          case js.Transient(UndefinedParam) =>
            unboxedArg
          case _ =>
            val tpe = paramTypes.getOrElse(paramName, paramType)
            box(unboxedArg, tpe)
        }
        reversedArgs ::= boxedArg
      }
    }

    /* Remove all consecutive UndefinedParam's at the end of the argument
     * list. No check is performed whether they may be there, since they will
     * only be placed where default arguments can be anyway.
     */
    reversedArgs = reversedArgs.dropWhile(_.isInstanceOf[js.Transient])

    /* Find remaining UndefinedParam and replace by js.Undefined. This can
     * happen with named arguments or with multiple argument lists.
     */
    reversedArgs = reversedArgs map {
      case js.Transient(UndefinedParam) => js.Undefined()
      case arg => arg
    }

    reversedArgs.reverse
  }

  /** Gen JS code for a repeated param of a JS method.
   *
   *  In this case `arg` has type `Seq[T]` for some `T`, but the result should
   *  be an expanded list of the elements in the sequence. So this method
   *  takes care of the conversion.
   *
   *  It is specialized for the shapes of tree generated by the desugaring
   *  of repeated params in Scala, so that these are actually expanded at
   *  compile-time.
   *
   *  Otherwise, it returns a `JSSpread` with the `Seq` converted to a
   *  `js.Array`.
   */
  private def genJSRepeatedParam(arg: Tree): List[js.TreeOrJSSpread] = {
    tryGenRepeatedParamAsJSArray(arg, handleNil = true).getOrElse {
      /* Fall back to calling runtime.genTraversableOnce2jsArray
       * to perform the conversion to js.Array, then wrap in a Spread
       * operator.
       */
      implicit val pos = arg.span
      val jsArrayArg = genModuleApplyMethod(
          jsdefn.Runtime_toJSVarArgs,
          List(genExpr(arg)))
      List(js.JSSpread(jsArrayArg))
    }
  }

  /** Try and expand an actual argument to a repeated param `(xs: T*)`.
   *
   *  This method recognizes the shapes of tree generated by the desugaring
   *  of repeated params in Scala, and expands them.
   *  If `arg` does not have the shape of a generated repeated param, this
   *  method returns `None`.
   */
  private def tryGenRepeatedParamAsJSArray(arg: Tree,
      handleNil: Boolean): Option[List[js.Tree]] = {
    implicit val pos = arg.span

    // Given a method `def foo(args: T*)`
    arg match {
      // foo(arg1, arg2, ..., argN) where N > 0
      case MaybeAsInstanceOf(WrapArray(MaybeAsInstanceOf(array: JavaSeqLiteral))) =>
        /* Value classes in arrays are already boxed, so no need to use
         * the type before erasure.
         * TODO Is this true in dotty?
         */
        Some(array.elems.map(e => box(genExpr(e), e.tpe)))

      // foo()
      case Ident(_) if handleNil && arg.symbol == defn.NilModule =>
        Some(Nil)

      // foo(argSeq: _*) - cannot be optimized
      case _ =>
        None
    }
  }

  private object MaybeAsInstanceOf {
    def unapply(tree: Tree): Some[Tree] = tree match {
      case TypeApply(asInstanceOf_? @ Select(base, _), _)
          if asInstanceOf_?.symbol == defn.Any_asInstanceOf =>
        Some(base)
      case _ =>
        Some(tree)
    }
  }

  private object WrapArray {
    lazy val isWrapArray: Set[Symbol] = {
      val names0 = defn.ScalaValueClasses().map(sym => nme.wrapXArray(sym.name))
      val names1 = names0 ++ Set(nme.wrapRefArray, nme.genericWrapArray)
      val names2 = names1.map(defn.ScalaPredefModule.requiredMethod(_))
      names2.toSet
    }

    def unapply(tree: Apply): Option[Tree] = tree match {
      case Apply(wrapArray_?, List(wrapped)) if isWrapArray(wrapArray_?.symbol) =>
        Some(wrapped)
      case _ =>
        None
    }
  }

  /** Gen JS code for loading a Java static field.
   */
  private def genLoadStaticField(sym: Symbol)(implicit pos: Position): js.Tree = {
    /* Actually, there is no static member in Scala.js. If we come here, that
     * is because we found the symbol in a Java-emitted .class in the
     * classpath. But the corresponding implementation in Scala.js will
     * actually be a val in the companion module.
     */

    if (sym == defn.BoxedUnit_UNIT) {
      js.Undefined()
    } else {
      val inst = genLoadModule(sym.owner)
      val method = encodeStaticMemberSym(sym)
      js.Apply(js.ApplyFlags.empty, inst, method, Nil)(toIRType(sym.info))
    }
  }

  /** Gen JS code for loading a module.
   *
   *  Can be given either the module symbol, or its module class symbol.
   */
  private def genLoadModule(sym0: Symbol)(implicit pos: Position): js.Tree = {
    require(sym0.is(Module),
        "genLoadModule called with non-module symbol: " + sym0)
    val sym = if (sym0.isTerm) sym0.moduleClass else sym0

    if (isJSType(sym)) {
      if (isScalaJSDefinedJSClass(sym))
        js.LoadJSModule(encodeClassRef(sym))
      /*else if (sym.derivesFrom(jsdefn.JSGlobalScopeClass))
        genLoadJSGlobal()*/
      else
        genLoadNativeJSModule(sym)
    } else {
      js.LoadModule(encodeClassRef(sym))
    }
  }

  /** Gen JS code representing the constructor of a JS class. */
  private def genLoadJSConstructor(sym: Symbol)(
      implicit pos: Position): js.Tree = {
    assert(!isStaticModule(sym) && !sym.is(Trait),
        s"genPrimitiveJSClass called with non-class $sym")
    js.LoadJSConstructor(encodeClassRef(sym))
  }

  /** Gen JS code representing a native JS module. */
  private def genLoadNativeJSModule(sym: Symbol)(
      implicit pos: Position): js.Tree = {
    require(sym.is(ModuleClass),
        s"genLoadNativeJSModule called with non-module $sym")
    fullJSNameOf(sym).split('.').foldLeft(genLoadJSGlobal()) { (memo, chunk) =>
      js.JSBracketSelect(memo, js.StringLiteral(chunk))
    }
  }

  /** Gen JS code to load the JavaScript global scope. */
  private def genLoadJSGlobal()(implicit pos: Position): js.Tree = {
    js.JSBracketSelect(
        js.JSBracketSelect(js.JSLinkingInfo(), js.StringLiteral("envInfo")),
        js.StringLiteral("global"))
  }

  /** Generate a Class[_] value (e.g. coming from classOf[T]) */
  private def genClassConstant(tpe: Type)(implicit pos: Position): js.Tree =
    js.ClassOf(toTypeRef(tpe))

  private def isStaticModule(sym: Symbol): Boolean =
    sym.is(Module) && sym.isStatic

  private def isPrimitiveValueType(tpe: Type): Boolean = {
    tpe.widenDealias match {
      case JavaArrayType(_) => false
      case t                => t.typeSymbol.asClass.isPrimitiveValueClass
    }
  }

  protected lazy val isHijackedClass: Set[Symbol] = {
    /* This list is a duplicate of ir.Definitions.HijackedClasses, but
     * with global.Symbol's instead of IR encoded names as Strings.
     * We also add java.lang.Void, which BoxedUnit "erases" to.
     */
    Set[Symbol](
        defn.BoxedUnitClass, defn.BoxedBooleanClass, defn.BoxedCharClass, defn.BoxedByteClass,
        defn.BoxedShortClass, defn.BoxedIntClass, defn.BoxedLongClass, defn.BoxedFloatClass,
        defn.BoxedDoubleClass, defn.StringClass, jsdefn.JavaLangVoidClass
    )
  }

  // Copied from DottyBackendInterface

  private val desugared = new java.util.IdentityHashMap[Type, tpd.Select]

  def desugarIdent(i: Ident): Option[tpd.Select] = {
    var found = desugared.get(i.tpe)
    if (found == null) {
      tpd.desugarIdent(i) match {
        case sel: tpd.Select =>
          desugared.put(i.tpe, sel)
          found = sel
        case _ =>
      }
    }
    if (found == null) None else Some(found)
  }
}

object JSCodeGen {

  /** Marker object for undefined parameters in JavaScript semantic calls.
   *
   *  To be used inside a `js.Transient` node.
   */
  case object UndefinedParam extends js.Transient.Value {
    def printIR(out: ir.Printers.IRTreePrinter): Unit =
      out.print("<undefined-param>")
  }

}
