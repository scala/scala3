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
import Flags._
import dotty.tools.dotc.ast.Trees._
import Types._
import Symbols._
import Denotations._
import Phases._
import dotty.tools.dotc.util.Positions
import Positions.Position
import StdNames._

import dotty.tools.dotc.transform.Erasure

import org.scalajs.core.ir
import org.scalajs.core.ir.{ClassKind, Trees => js, Types => jstpe}
import js.OptimizerHints

import JSEncoding._
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

  /** Implicitly materializes the current local name generator. */
  private implicit def implicitLocalNames: LocalNameGenerator = localNames.get

  /* See genSuperCall()
   * TODO Can we avoid this unscoped var?
   */
  private var isModuleInitialized: Boolean = false

  private def currentClassType = encodeClassType(currentClassSym)

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
      implicit val pos: Position = sym.pos

      /* Do not actually emit code for primitive types nor scala.Array. */
      val isPrimitive =
        sym.isPrimitiveValueClass || sym == defn.ArrayClass

      if (!isPrimitive) {
        withScopedVars(
            currentClassSym := sym
        ) {
          val tree = if (isRawJSType(sym)) {
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

    for ((sym, tree) <- generatedClasses) {
      val writer = new java.io.PrintWriter(System.err)
      try {
        new ir.Printers.IRTreePrinter(writer).print(tree)
      } finally {
        writer.flush()
      }
      genIRFile(cunit, sym, tree)
    }
  }

  private def genIRFile(cunit: CompilationUnit, sym: Symbol,
      tree: ir.Trees.ClassDef): Unit = {
    val outfile = getFileFor(cunit, sym, ".sjsir")
    val output = outfile.bufferedOutput
    try {
      ir.InfoSerializers.serialize(output, ir.Infos.generateClassInfo(tree))
      ir.Serializers.serialize(output, tree)
    } finally {
      output.close()
    }
  }

  private def getFileFor(cunit: CompilationUnit, sym: Symbol,
      suffix: String) = {
    import scala.reflect.io._

    val outputDirectory: AbstractFile = // TODO Support virtual files
      new PlainDirectory(new Directory(new java.io.File(ctx.settings.d.value)))

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
    implicit val pos: Position = sym.pos

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
      // Hack to export hello.world
      if (sym.fullName.toString == "hello.world$") {
        List(
          js.ModuleExportDef("hello.world"),
          js.MethodDef(static = false, js.StringLiteral("main"),
              Nil, jstpe.AnyType,
              js.Block(List(
                js.Apply(js.This()(jstpe.ClassType(classIdent.name)), js.Ident("main__V"), Nil)(jstpe.NoType),
                js.Undefined())))(
              OptimizerHints.empty, None))
      } else {
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
    }

    // Hashed definitions of the class
    val hashedDefs =
      ir.Hashers.hashDefs(generatedMembers ++ exports)

    // The complete class definition
    val kind =
      if (isStaticModule(sym)) ClassKind.ModuleClass
      else if (isHijacked) ClassKind.HijackedClass
      else ClassKind.Class

    val classDefinition = js.ClassDef(
        classIdent,
        kind,
        Some(encodeClassFullNameIdent(sym.superClass)),
        genClassInterfaces(sym),
        None,
        hashedDefs)(
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
    ???
  }

  /** Gen the IR ClassDef for an interface definition.
   */
  private def genInterface(td: TypeDef): js.ClassDef = {
    val sym = td.symbol.asClass
    implicit val pos: Position = sym.pos

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
      ir.Hashers.hashDefs(generatedMethods.toList)

    js.ClassDef(classIdent, ClassKind.Interface, None, superInterfaces, None,
        hashedDefs)(OptimizerHints.empty)
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
    (for {
      f <- classSym.info.decls
      if !f.is(Method) && f.isTerm
    } yield {
      implicit val pos: Position = f.pos

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

      js.FieldDef(name, irTpe, f.is(Mutable))
    }).toList
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
    implicit val pos: Position = dd.pos
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
        implicit val pos: Position = param.pos
        js.ParamDef(encodeLocalSym(param), toIRType(param.info),
            mutable = false, rest = false)
      }

      /*if (primitives.isPrimitive(sym)) {
        None
      } else*/ if (sym.is(Deferred)) {
        Some(js.MethodDef(static = false, methodName,
            jsParams, toIRType(patchedResultType(sym)), js.EmptyTree)(
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
            js.MethodDef(static = false, methodName,
                jsParams, jstpe.NoType, body1)(optimizerHints, None)
          } else*/ if (sym.isConstructor) {
            js.MethodDef(static = false, methodName,
                jsParams, jstpe.NoType,
                genStat(rhs))(optimizerHints, None)
          } else {
            val resultIRType = toIRType(patchedResultType(sym))
            genMethodDef(static = false, methodName,
                params, resultIRType, rhs, optimizerHints)
          }
        }

        /* Work around https://github.com/scala-js/scala-js/issues/2259
         * TODO Remove this when we upgrade to Scala.js 0.6.8.
         */
        val methodDef1 = if (!sym.owner.is(Trait)) {
          methodDef
        } else {
          val workaroundBody = js.Block(
              js.Apply(js.ClassOf(jstpe.ClassType(encodeClassFullName(sym.owner))),
                  js.Ident("isPrimitive__Z"), Nil)(jstpe.BooleanType),
              methodDef.body)
          methodDef.copy(body = workaroundBody)(
              methodDef.optimizerHints, methodDef.hash)
        }

        Some(methodDef1)
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
  private def genMethodDef(static: Boolean, methodName: js.PropertyName,
      paramsSyms: List[Symbol], resultIRType: jstpe.Type,
      tree: Tree, optimizerHints: OptimizerHints): js.MethodDef = {
    implicit val pos: Position = tree.pos

    ctx.debuglog("genMethod " + methodName.name)
    ctx.debuglog("")

    val jsParams = for (param <- paramsSyms) yield {
      implicit val pos: Position = param.pos
      js.ParamDef(encodeLocalSym(param), toIRType(param.info),
          mutable = false, rest = false)
    }

    def genBody() =
      if (resultIRType == jstpe.NoType) genStat(tree)
      else genExpr(tree)

    //if (!isScalaJSDefinedJSClass(currentClassSym)) {
      js.MethodDef(static, methodName, jsParams, resultIRType, genBody())(
          optimizerHints, None)
    /*} else {
      assert(!static, tree.pos)

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
    implicit val pos: ir.Position = tree.pos
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
        s"genExpr($tree) returned a tree with type NoType at pos ${tree.pos}")
    result
  }

  /** Gen JS code for a tree in statement or expression position (in the IR).
   *
   *  This is the main transformation method. Each node of the Scala AST
   *  is transformed into an equivalent portion of the JS AST.
   */
  private def genStatOrExpr(tree: Tree, isStat: Boolean): js.Tree = {
    implicit val pos: Position = tree.pos

    ctx.debuglog("  " + tree)
    ctx.debuglog("")

    tree match {
      /** LabelDefs (for while and do..while loops) */
      /*case lblDf: LabelDef =>
        genLabelDef(lblDf)*/

      /** Local val or var declaration */
      case tree @ ValDef(name, _, _) =>
        /* Must have been eliminated by the tail call transform performed
         * by genMethodBody(). */
        assert(name != nme.THIS,
            s"ValDef(_, nme.THIS, _, _) found at ${tree.pos}")

        val sym = tree.symbol
        val rhs = tree.rhs
        val rhsTree = genExpr(rhs)

        rhsTree match {
          case js.UndefinedParam() =>
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

      case Return(expr, from) =>
        // TODO Need to consider `from`?
        js.Return(toIRType(expr.tpe) match {
          case jstpe.NoType => js.Block(genStat(expr), js.Undefined())
          case _            => genExpr(expr)
        })

      /*case t: Try =>
        genTry(t, isStat)*/

      /*case Throw(expr) =>
        val ex = genExpr(expr)
        js.Throw {
          if (isMaybeJavaScriptException(expr.tpe)) {
            genApplyMethod(
                genLoadModule(RuntimePackageModule),
                Runtime_unwrapJavaScriptException,
                List(ex))
          } else {
            ex
          }
        }*/

      case app: Apply =>
        genApply(app, isStat)

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
              " pos:" + pos)
          genLoadModule(tree.symbol)
        }

      case Select(qualifier, _) =>
        val sym = tree.symbol
        if (sym.is(Module)) {
          assert(!sym.is(Package), "Cannot use package as value: " + tree)
          genLoadModule(sym)
        } else /*if (sym.isStaticMember) {
          genStaticMember(sym)
        } else if (paramAccessorLocals contains sym) {
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
            js.UndefinedParam()(toIRType(sym.info))
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
      /*case av: ArrayValue =>
        genArrayValue(av)

      /** A Match reaching the backend is supposed to be optimized as a switch */
      case mtch: Match =>
        genMatch(mtch, isStat)

      /** Anonymous function (only with -Ydelambdafy:method) */
      case fun: Function =>
        genAnonFunction(fun)

      case EmptyTree =>
        js.Skip()*/

      case _ =>
        throw new FatalError("Unexpected tree in genExpr: " +
            tree + "/" + tree.getClass + " at: " + tree.pos)
    }
  } // end of genStatOrExpr()

  // !!! DUPLICATE code with DottyBackendInterface
  private def desugarIdent(i: Ident): Option[Select] = {
    i.tpe match {
      case TermRef(prefix: TermRef, name) =>
        Some(tpd.ref(prefix).select(i.symbol))
      case TermRef(prefix: ThisType, name) =>
        Some(tpd.This(prefix.cls).select(i.symbol))
      /*case TermRef(NoPrefix, name) =>
        if (i.symbol is Method) Some(This(i.symbol.topLevelClass).select(i.symbol)) // workaround #342 todo: remove after fixed
        else None*/
      case _ =>
        None
    }
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
    implicit val pos: Position = tree.pos
    val args = tree.args
    val sym = tree.fun.symbol

    val fun = tree.fun match {
      case fun: Ident => desugarIdent(fun).getOrElse(fun)
      case fun => fun
    }

    def isRawJSDefaultParam: Boolean = {
      false /*
      if (isCtorDefaultParam(sym)) {
        isRawJSCtorDefaultParam(sym)
      } else {
        sym.hasFlag(reflect.internal.Flags.DEFAULTPARAM) &&
        isRawJSType(sym.owner.tpe)
      }*/
    }

    fun match {
      /*case _: TypeApply =>
        genApplyTypeApply(tree)*/

      /*case _ if isRawJSDefaultParam =>
        js.UndefinedParam()(toIRType(sym.tpe.resultType))*/

      case Select(Super(_, _), _) =>
        genSuperCall(tree, isStat)

      /*case Select(New(_), nme.CONSTRUCTOR) =>
        genApplyNew(tree)*/

      case _ =>
        /*if (sym.isLabel) {
          genLabelApply(tree)
        } else if (primitives.isPrimitive(tree)) {
          genPrimitiveOp(tree, isStat)
        } else*/ if (Erasure.Boxing.isBox(sym)) {
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
    implicit val pos: Position = tree.pos
    val Apply(fun @ Select(sup @ Super(_, mix), _), args) = tree
    val sym = fun.symbol

    if (sym == defn.Any_getClass) {
      // The only primitive that is also callable as super call
      js.GetClass(genThis())
    } else /*if (isScalaJSDefinedJSClass(currentClassSym)) {
      genJSSuperCall(tree, isStat)
    } else*/ {
      val superCall = genApplyMethodStatically(
          genThis()(sup.pos), sym, genActualArgs(sym, args))

      // Initialize the module instance just after the super constructor call.
      if (isStaticModule(currentClassSym) && !isModuleInitialized &&
          currentMethodSym.get.isClassConstructor) {
        isModuleInitialized = true
        val thisType = jstpe.ClassType(encodeClassFullName(currentClassSym))
        val initModule = js.StoreModule(thisType, js.This()(thisType))
        js.Block(superCall, initModule)
      } else {
        superCall
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
    implicit val pos: Position = tree.pos

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

    /*if (sym.owner == defn.StringClass && !isStringMethodFromObject) {
      genStringCall(tree)
    } else if (isRawJSType(sym.owner)) {
      if (!isScalaJSDefinedJSClass(sym.owner) || isExposed(sym))
        genPrimitiveJSCall(tree, isStat)
      else
        genApplyJSClassMethod(genExpr(receiver), sym, genActualArgs(sym, args))
    } else*/ if (foreignIsImplClass(sym.owner)) {
      genTraitImplApply(sym, args.map(genExpr))
    } else if (sym.isClassConstructor) {
      // Calls to constructors are always statically linked
      genApplyMethodStatically(genExpr(receiver), sym, genActualArgs(sym, args))
    } else {
      genApplyMethod(genExpr(receiver), sym, genActualArgs(sym, args))
    }
  }

  /** Gen a dynamically linked call to a Scala method. */
  private def genApplyMethod(receiver: js.Tree,
      methodSym: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    js.Apply(receiver, encodeMethodSym(methodSym), arguments)(
        toIRType(patchedResultType(methodSym)))
  }

  /** Gen a statically linked call to an instance method. */
  private def genApplyMethodStatically(receiver: js.Tree, method: Symbol,
      arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
    val className = encodeClassFullName(method.owner)
    val methodIdent = encodeMethodSym(method)
    val resultType = toIRType(patchedResultType(method))
    js.ApplyStatically(receiver, jstpe.ClassType(className),
        methodIdent, arguments)(resultType)
  }

  /** Gen a call to a static method. */
  private def genApplyStatic(method: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    val cls = jstpe.ClassType(encodeClassFullName(method.owner))
    val methodIdent = encodeMethodSym(method)
    js.ApplyStatic(cls, methodIdent, arguments)(
        toIRType(patchedResultType(method)))
  }

  /** Gen a call to a Scala2 impl class method. */
  private def genTraitImplApply(method: Symbol, arguments: List[js.Tree])(
      implicit pos: Position): js.Tree = {
    genApplyStatic(method, arguments)
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
    toReferenceType(tpe) match {
      case jstpe.ClassType(cls) if ir.Definitions.isPrimitiveClass(cls) =>
        assert(cls.length == 1)
        (cls.charAt(0): @switch) match {
          case 'V' =>
            // must be handled at least for JS interop
            js.Block(expr, js.Undefined())
          case 'C' =>
            genModuleApplyMethod(jsdefn.BoxesRunTime_boxToCharacter, List(expr))
          case _ =>
            expr // box is identity for all non-Char types
        }

      case _ =>
        throw new FatalError(
            s"makePrimitiveBox requires a primitive type, found $tpe at $pos")
    }
  }

  /** Gen an unboxing operation (tpe is the primitive type) */
  private def makePrimitiveUnbox(expr: js.Tree, tpe: Type)(
      implicit pos: Position): js.Tree = {
    toReferenceType(tpe) match {
      case jstpe.ClassType(cls) if ir.Definitions.isPrimitiveClass(cls) =>
        assert(cls.length == 1)
        (cls.charAt(0): @switch) match {
          case 'V' =>
            // must be handled at least for JS interop
            expr
          case 'C' =>
            genModuleApplyMethod(jsdefn.BoxesRunTime_unboxToChar, List(expr))
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

  /** Generate loading of a module value
   *  Can be given either the module symbol, or its module class symbol.
   */
  private def genLoadModule(sym0: Symbol)(implicit pos: Position): js.Tree = {
    require(sym0.is(Module),
        "genLoadModule called with non-module symbol: " + sym0)
    val sym1 = if (sym0.isTerm) sym0.moduleClass else sym0
    val sym = // redirect all static methods of String to RuntimeString
      if (sym1 == defn.StringModule) jsdefn.RuntimeStringModule.moduleClass
      else sym1

    //val isGlobalScope = sym.tpe.typeSymbol isSubClass JSGlobalScopeClass

    /*if (isGlobalScope) {
      genLoadGlobal()
    } else if (isJSNativeClass(sym)) {
      genPrimitiveJSModule(sym)
    } else {*/
      val cls = jstpe.ClassType(encodeClassFullName(sym))
      if (isRawJSType(sym)) js.LoadJSModule(cls)
      else js.LoadModule(cls)
    //}
  }

  /** Generate a Class[_] value (e.g. coming from classOf[T]) */
  private def genClassConstant(tpe: Type)(implicit pos: Position): js.Tree =
    js.ClassOf(toReferenceType(tpe))

  private def isStaticModule(sym: Symbol): Boolean =
    sym.is(Module) && sym.isStatic

}
