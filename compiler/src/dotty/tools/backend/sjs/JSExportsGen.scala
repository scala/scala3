package dotty.tools.backend.sjs

import scala.annotation.tailrec

import scala.collection.mutable

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core._

import Contexts._
import Decorators._
import Denotations._
import Flags._
import Names._
import NameKinds.DefaultGetterName
import Periods._
import Phases._
import StdNames._
import Symbols._
import SymDenotations._
import Types._
import TypeErasure.ErasedValueType

import dotty.tools.dotc.transform.Erasure
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.report

import org.scalajs.ir
import org.scalajs.ir.{ClassKind, Position, Names => jsNames, Trees => js, Types => jstpe}
import org.scalajs.ir.Names.{ClassName, MethodName, SimpleMethodName}
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.Trees.OptimizerHints

import dotty.tools.dotc.transform.sjs.JSSymUtils._

import JSEncoding._

final class JSExportsGen(jsCodeGen: JSCodeGen)(using Context) {
  import jsCodeGen._
  import positionConversions._

  def genJSClassDispatchers(classSym: Symbol, dispatchMethodsNames: List[JSName]): List[js.MemberDef] = {
    dispatchMethodsNames.map(genJSClassDispatcher(classSym, _))
  }

  private def genJSClassDispatcher(classSym: Symbol, name: JSName): js.MemberDef = {
    val alts = classSym.info.membersBasedOnFlags(required = Method, excluded = Bridge)
      .map(_.symbol)
      .filter { sym =>
        /* scala-js#3939: Object is not a "real" superclass of JS types.
         * as such, its methods do not participate in overload resolution.
         * An exception is toString, which is handled specially in genExportMethod.
         */
        sym.owner != defn.ObjectClass && sym.jsName == name
      }
      .toList

    assert(!alts.isEmpty, s"Ended up with no alternatives for ${classSym.fullName}::$name.")

    val (propSyms, methodSyms) = alts.partition(_.isJSProperty)
    val isProp = propSyms.nonEmpty

    if (isProp && methodSyms.nonEmpty) {
      val firstAlt = alts.head
      report.error(
          i"Conflicting properties and methods for ${classSym.fullName}::$name.",
          firstAlt.srcPos)
      implicit val pos = firstAlt.span
      js.JSPropertyDef(js.MemberFlags.empty, genExpr(name)(firstAlt.sourcePos), None, None)
    } else {
      genMemberExportOrDispatcher(name, isProp, alts, static = false)
    }
  }

  private def genMemberExportOrDispatcher(jsName: JSName, isProp: Boolean,
      alts: List[Symbol], static: Boolean): js.MemberDef = {
    withNewLocalNameScope {
      if (isProp)
        genExportProperty(alts, jsName, static)
      else
        genExportMethod(alts.map(Exported), jsName, static)
    }
  }

  def genJSConstructorDispatch(alts: List[Symbol]): (Option[List[js.ParamDef]], js.JSMethodDef) = {
    val exporteds = alts.map(Exported)

    val isConstructorOfNestedJSClass = exporteds.head.isConstructorOfNestedJSClass
    assert(exporteds.tail.forall(_.isConstructorOfNestedJSClass == isConstructorOfNestedJSClass),
        s"Alternative constructors $alts do not agree on whether they are in a nested JS class or not")
    val captureParams = if (!isConstructorOfNestedJSClass) {
      None
    } else {
      Some(for {
        exported <- exporteds
        param <- exported.captureParamsFront ::: exported.captureParamsBack
      } yield {
        param
      })
    }

    val ctorDef = genExportMethod(exporteds, JSName.Literal("constructor"), static = false)

    (captureParams, ctorDef)
  }

  private def genExportProperty(alts: List[Symbol], jsName: JSName, static: Boolean): js.JSPropertyDef = {
    assert(!alts.isEmpty, s"genExportProperty with empty alternatives for $jsName")

    implicit val pos: Position = alts.head.span

    val namespace =
      if (static) js.MemberNamespace.PublicStatic
      else js.MemberNamespace.Public
    val flags = js.MemberFlags.empty.withNamespace(namespace)

    /* Separate getters and setters. Since we only have getters and setters, we
     * simply test the param list size, which is faster than using the full isJSGetter.
     */
    val (getter, setters) = alts.partition(_.info.paramInfoss.head.isEmpty)

    // We can have at most one getter
    if (getter.sizeIs > 1) {
      /* Member export of properties should be caught earlier, so if we get
       * here with a non-static export, something went horribly wrong.
       */
      assert(static, s"Found more than one instance getter to export for name $jsName.")
      for (duplicate <- getter.tail)
        report.error(s"Duplicate static getter export with name '${jsName.displayName}'", duplicate)
    }

    val getterBody = getter.headOption.map { getterSym =>
      genApplyForSingleExported(new FormalArgsRegistry(0, false), Exported(getterSym), static)
    }

    val setterArgAndBody = {
      if (setters.isEmpty) {
        None
      } else {
        val formalArgsRegistry = new FormalArgsRegistry(1, false)
        val List(arg) = formalArgsRegistry.genFormalArgs()
        val body = genExportSameArgc(jsName, formalArgsRegistry, setters.map(Exported), static, None)
        Some((arg, body))
      }
    }

    js.JSPropertyDef(flags, genExpr(jsName)(alts.head.sourcePos), getterBody, setterArgAndBody)
  }

  private def genExportMethod(alts0: List[Exported], jsName: JSName, static: Boolean): js.JSMethodDef = {
    assert(alts0.nonEmpty, "need at least one alternative to generate exporter method")

    implicit val pos = alts0.head.pos

    val namespace =
      if (static) js.MemberNamespace.PublicStatic
      else js.MemberNamespace.Public
    val flags = js.MemberFlags.empty.withNamespace(namespace)

    // toString() is always exported. We might need to add it here to get correct overloading.
    val alts = jsName match {
      case JSName.Literal("toString") if alts0.forall(_.params.nonEmpty) =>
        Exported(defn.Any_toString) :: alts0
      case _ =>
        alts0
    }

    val (formalArgs, body) =
      if (alts.tail.isEmpty) genExportMethodSingleAlt(alts.head, static)
      else genExportMethodMultiAlts(alts, jsName, static)

    js.JSMethodDef(flags, genExpr(jsName), formalArgs, body)(OptimizerHints.empty, None)
  }

  private def genExportMethodSingleAlt(alt: Exported, static: Boolean)(
      implicit pos: SourcePosition): (List[js.ParamDef], js.Tree) = {
    /* This is a fast path for `genExportMethod` that applies for all methods that
     * are not overloaded. In addition to being a fast path, it does a better job
     * than `genExportMethodMultiAlts` when the only alternative has default
     * parameters, because it avoids a spurious dispatch.
     * In scalac, the spurious dispatch was avoided by a more elaborate case
     * generation in `genExportMethod`, which was very convoluted and was not
     * ported to dotc.
     */

    val params = alt.params
    val paramsSize = params.size

    val minArgc = {
      // Find the first default param or repeated param
      val firstOptionalParamIndex = params.indexWhere(p => p.hasDefault || p.isRepeated)
      if (firstOptionalParamIndex == -1) paramsSize
      else firstOptionalParamIndex
    }

    val hasVarArg = alt.hasRepeatedParam
    val maxArgc = if (hasVarArg) paramsSize - 1 else paramsSize
    val needsRestParam = maxArgc != minArgc || hasVarArg
    val formalArgsRegistry = new FormalArgsRegistry(minArgc, needsRestParam)

    val formalArgs = formalArgsRegistry.genFormalArgs()
    val body = genApplyForSingleExported(formalArgsRegistry, alt, static)

    (formalArgs, body)
  }

  private def genExportMethodMultiAlts(alts: List[Exported], jsName: JSName, static: Boolean)(
      implicit pos: SourcePosition): (List[js.ParamDef], js.Tree) = {
    // Factor out methods with variable argument lists.
    // They can only be at the end of the lists as enforced by PrepJSExports.
    val (varArgMeths, normalMeths) = alts.partition(_.hasRepeatedParam)

    // Highest non-repeated argument count
    // For varArgsMeths, we have argc - 1, since a repeated parameter list may also be empty (unlike a normal parameter)
    val maxArgc = (varArgMeths.map(_.params.size - 1) ::: normalMeths.map(_.params.size)).max

    // Calculates possible arg counts for normal method
    def argCounts(ex: Exported): Seq[Int] = {
      val params = ex.params
      // Find default param
      val dParam = params.indexWhere(_.hasDefault)
      if (dParam == -1) Seq(params.size)
      else dParam to params.size
    }

    // Generate tuples (argc, method)
    val methodArgCounts = {
      // Normal methods
      for {
        method <- normalMeths
        argc <- argCounts(method)
      } yield (argc, method)
    } ::: {
      // Repeated parameter methods
      for {
        method <- varArgMeths
        argc <- method.params.size - 1 to maxArgc
      } yield (argc, method)
    }

    // Create the formal args registry
    val minArgc = methodArgCounts.minBy(_._1)._1
    val hasVarArg = varArgMeths.nonEmpty
    val needsRestParam = maxArgc != minArgc || hasVarArg
    val formalArgsRegistry = new FormalArgsRegistry(minArgc, needsRestParam)

    // List of formal parameters
    val formalArgs = formalArgsRegistry.genFormalArgs()

    // Create a list of (argCount -> methods), sorted by argCount (methods may appear multiple times)
    val methodByArgCount: List[(Int, List[Exported])] =
      methodArgCounts.groupMap(_._1)(_._2).toList.sortBy(_._1) // sort for determinism

    // Generate a case block for each (argCount, methods) tuple
    // TODO? We could optimize this a bit by putting together all the `argCount`s that have the same methods
    // (Scala.js for scalac does that, but the code is very convoluted and it's not clear that it is worth it).
    val cases = for {
      (argc, methods) <- methodByArgCount
      if methods != varArgMeths // exclude default case we're generating anyways for varargs
    } yield {
      // body of case to disambiguates methods with current count
      val caseBody = genExportSameArgc(jsName, formalArgsRegistry, methods, static, Some(argc))
      List(js.IntLiteral(argc - minArgc)) -> caseBody
    }

    def defaultCase = {
      if (!hasVarArg)
        genThrowTypeError()
      else
        genExportSameArgc(jsName, formalArgsRegistry, varArgMeths, static, None)
    }

    val body = {
      if (cases.isEmpty) {
        defaultCase
      } else if (cases.tail.isEmpty && !hasVarArg) {
        cases.head._2
      } else {
        assert(needsRestParam, "Trying to read rest param length but needsRestParam is false")
        val restArgRef = formalArgsRegistry.genRestArgRef()
        js.Match(
            js.AsInstanceOf(js.JSSelect(restArgRef, js.StringLiteral("length")), jstpe.IntType),
            cases.toList, defaultCase)(jstpe.AnyType)
      }
    }

    (formalArgs, body)
  }

  /** Resolves method calls to [[alts]] while assuming they have the same parameter count.
   *
   *  @param jsName
   *    The JS name of the method, for error reporting
   *  @param formalArgsRegistry
   *    The registry of all the formal arguments
   *  @param alts
   *    Alternative methods
   *  @param static
   *    Whether we are generating a static method
   *  @param maxArgc
   *    Maximum number of arguments to use for disambiguation
   */
  private def genExportSameArgc(jsName: JSName, formalArgsRegistry: FormalArgsRegistry,
      alts: List[Exported], static: Boolean, maxArgc: Option[Int]): js.Tree = {
    genExportSameArgcRec(jsName, formalArgsRegistry, alts, paramIndex = 0, static, maxArgc)
  }

  /** Resolves method calls to [[alts]] while assuming they have the same parameter count.
   *
   *  @param jsName
   *    The JS name of the method, for error reporting
   *  @param formalArgsRegistry
   *    The registry of all the formal arguments
   *  @param alts
   *    Alternative methods
   *  @param paramIndex
   *    Index where to start disambiguation (starts at 0, increases through recursion)
   *  @param static
   *    Whether we are generating a static method
   *  @param maxArgc
   *    Maximum number of arguments to use for disambiguation
   */
  private def genExportSameArgcRec(jsName: JSName, formalArgsRegistry: FormalArgsRegistry, alts: List[Exported],
      paramIndex: Int, static: Boolean, maxArgc: Option[Int]): js.Tree = {

    implicit val pos = alts.head.pos

    if (alts.sizeIs == 1) {
      genApplyForSingleExported(formalArgsRegistry, alts.head, static)
    } else if (maxArgc.exists(_ <= paramIndex) || !alts.exists(_.params.size > paramIndex)) {
      // We reach here in three cases:
      // 1. The parameter list has been exhausted
      // 2. The optional argument count restriction has triggered
      // 3. We only have (more than once) repeated parameters left
      // Therefore, we should fail
      reportCannotDisambiguateError(jsName, alts)
      js.Undefined()
    } else {
      val altsByTypeTest = groupByWithoutHashCode(alts) { exported =>
        typeTestForTpe(exported.exportArgTypeAt(paramIndex))
      }

      if (altsByTypeTest.size == 1) {
        // Testing this parameter is not doing any us good
        genExportSameArgcRec(jsName, formalArgsRegistry, alts, paramIndex + 1, static, maxArgc)
      } else {
        // Sort them so that, e.g., isInstanceOf[String] comes before isInstanceOf[Object]
        val sortedAltsByTypeTest = topoSortDistinctsBy(altsByTypeTest)(_._1)

        val defaultCase = genThrowTypeError()

        sortedAltsByTypeTest.foldRight[js.Tree](defaultCase) { (elem, elsep) =>
          val (typeTest, subAlts) = elem
          implicit val pos = subAlts.head.pos

          val paramRef = formalArgsRegistry.genArgRef(paramIndex)
          val genSubAlts = genExportSameArgcRec(jsName, formalArgsRegistry,
              subAlts, paramIndex + 1, static, maxArgc)

          def hasDefaultParam = subAlts.exists { exported =>
            val params = exported.params
            params.size > paramIndex &&
            params(paramIndex).hasDefault
          }

          val optCond = typeTest match {
            case PrimitiveTypeTest(tpe, _) => Some(js.IsInstanceOf(paramRef, tpe))
            case InstanceOfTypeTest(tpe)   => Some(genIsInstanceOf(paramRef, tpe))
            case NoTypeTest                => None
          }

          optCond.fold[js.Tree] {
            genSubAlts // note: elsep is discarded, obviously
          } { cond =>
            val condOrUndef = if (!hasDefaultParam) cond else {
              js.If(cond, js.BooleanLiteral(true),
                  js.BinaryOp(js.BinaryOp.===, paramRef, js.Undefined()))(
                  jstpe.BooleanType)
            }
            js.If(condOrUndef, genSubAlts, elsep)(jstpe.AnyType)
          }
        }
      }
    }
  }

  private def reportCannotDisambiguateError(jsName: JSName, alts: List[Exported]): Unit = {
    val currentClass = currentClassSym.get

    /* Find a position that is in the current class for decent error reporting.
     * If there are more than one, always use the "highest" one (i.e., the
     * one coming last in the source text) so that we reliably display the
     * same error in all compilers.
     */
    val validPositions = alts.collect {
      case alt if alt.sym.owner == currentClass => alt.pos
    }
    val pos: SourcePosition =
      if (validPositions.isEmpty) currentClass.sourcePos
      else validPositions.maxBy(_.point)

    val kind =
      if (currentClass.isJSType) "method"
      else "exported method"

    val displayName = jsName.displayName
    val altsTypesInfo = alts.map(_.typeInfo).mkString("\n  ")

    report.error(
        s"Cannot disambiguate overloads for $kind $displayName with types\n  $altsTypesInfo",
        pos)
  }

  /** Generates a call to the method represented by the given `exported` while using the formalArguments
   *  and potentially the argument array.
   *
   *  Also inserts default parameters if required.
   */
  private def genApplyForSingleExported(formalArgsRegistry: FormalArgsRegistry,
      exported: Exported, static: Boolean): js.Tree = {
    if (currentClassSym.isJSType && exported.sym.owner != currentClassSym.get) {
      assert(!static, s"nonsensical JS super call in static export of ${exported.sym}")
      genApplyForSingleExportedJSSuperCall(formalArgsRegistry, exported)
    } else {
      genApplyForSingleExportedNonJSSuperCall(formalArgsRegistry, exported, static)
    }
  }

  private def genApplyForSingleExportedJSSuperCall(
      formalArgsRegistry: FormalArgsRegistry, exported: Exported): js.Tree = {
    implicit val pos = exported.pos

    val sym = exported.sym
    assert(!sym.isClassConstructor,
        s"Trying to genApplyForSingleExportedJSSuperCall for the constructor ${sym.fullName}")

    val allArgs = formalArgsRegistry.genAllArgsRefsForForwarder()

    val superClass = {
      val superClassSym = currentClassSym.asClass.superClass
      if (superClassSym.isNestedJSClass)
        js.VarRef(js.LocalIdent(JSSuperClassParamName))(jstpe.AnyType)
      else
        js.LoadJSConstructor(encodeClassName(superClassSym))
    }

    val receiver = js.This()(jstpe.AnyType)
    val nameTree = genExpr(sym.jsName)

    if (sym.isJSGetter) {
      assert(allArgs.isEmpty,
          s"getter symbol $sym does not have a getter signature")
      js.JSSuperSelect(superClass, receiver, nameTree)
    } else if (sym.isJSSetter) {
      assert(allArgs.size == 1 && allArgs.head.isInstanceOf[js.Tree],
          s"setter symbol $sym does not have a setter signature")
      js.Assign(js.JSSuperSelect(superClass, receiver, nameTree),
          allArgs.head.asInstanceOf[js.Tree])
    } else {
      js.JSSuperMethodCall(superClass, receiver, nameTree, allArgs)
    }
  }

  private def genApplyForSingleExportedNonJSSuperCall(
      formalArgsRegistry: FormalArgsRegistry, exported: Exported, static: Boolean): js.Tree = {

    implicit val pos = exported.pos

    // the (single) type of the repeated parameter if any
    val repeatedTpe = exported.params.lastOption.withFilter(_.isRepeated).map(_.info)

    val normalArgc = exported.params.size - (if (repeatedTpe.isDefined) 1 else 0)

    // optional repeated parameter list
    val jsVarArgPrep = repeatedTpe map { tpe =>
      val rhs = genJSArrayToVarArgs(formalArgsRegistry.genVarargRef(normalArgc))
      val ident = freshLocalIdent("prep" + normalArgc)
      js.VarDef(ident, NoOriginalName, rhs.tpe, mutable = false, rhs)
    }

    // normal arguments
    val jsArgRefs =
      (0 until normalArgc).toList.map(formalArgsRegistry.genArgRef(_))

    // Generate JS code to prepare arguments (default getters and unboxes)
    val jsArgPrep = genPrepareArgs(jsArgRefs, exported, static) ++ jsVarArgPrep
    val jsArgPrepRefs = jsArgPrep.map(_.ref)

    // Combine prep'ed formal arguments with captures
    val allJSArgs = {
      exported.captureParamsFront.map(_.ref) :::
      jsArgPrepRefs :::
      exported.captureParamsBack.map(_.ref)
    }

    val jsResult = genResult(exported, allJSArgs, static)

    js.Block(jsArgPrep :+ jsResult)
  }

  /** Generate the necessary JavaScript code to prepare the arguments of an
   *  exported method (unboxing and default parameter handling)
   */
  private def genPrepareArgs(jsArgs: List[js.Tree], exported: Exported, static: Boolean)(
      implicit pos: SourcePosition): List[js.VarDef] = {

    val result = new mutable.ListBuffer[js.VarDef]

    for {
      (jsArg, (param, i)) <- jsArgs.zip(exported.params.zipWithIndex)
    } yield {
      // Unboxed argument (if it is defined)
      val unboxedArg = unbox(jsArg, param.info)

      // If argument is undefined and there is a default getter, call it
      val verifiedOrDefault = if (param.hasDefault) {
        js.If(js.BinaryOp(js.BinaryOp.===, jsArg, js.Undefined()), {
          genCallDefaultGetter(exported.sym, i, param.sym.sourcePos, static) {
            prevArgsCount => result.take(prevArgsCount).toList.map(_.ref)
          }
        }, {
          // Otherwise, unbox the argument
          unboxedArg
        })(unboxedArg.tpe)
      } else {
        // Otherwise, it is always the unboxed argument
        unboxedArg
      }

      result += js.VarDef(freshLocalIdent("prep" + i), NoOriginalName,
          verifiedOrDefault.tpe, mutable = false, verifiedOrDefault)
    }

    result.toList
  }

  private def genCallDefaultGetter(sym: Symbol, paramIndex: Int, paramPos: SourcePosition, static: Boolean)(
      previousArgsValues: Int => List[js.Tree])(
      implicit pos: SourcePosition): js.Tree = {

    val targetSym = targetSymForDefaultGetter(sym)
    val defaultGetterDenot = this.defaultGetterDenot(targetSym, sym, paramIndex)

    assert(defaultGetterDenot.exists, s"need default getter for method ${sym.fullName}")
    assert(!defaultGetterDenot.isOverloaded, i"found overloaded default getter $defaultGetterDenot")
    val defaultGetter = defaultGetterDenot.symbol

    val targetTree =
      if (sym.isClassConstructor || static) genLoadModule(targetSym)
      else js.This()(encodeClassType(targetSym))

    // Pass previous arguments to defaultGetter
    val defaultGetterArgs = previousArgsValues(defaultGetter.info.paramInfoss.head.size)

    if (targetSym.isJSType) {
      if (defaultGetter.owner.isNonNativeJSClass) {
        genApplyJSClassMethod(targetTree, defaultGetter, defaultGetterArgs)
      } else {
        report.error(
            "When overriding a native method with default arguments, " +
            "the overriding method must explicitly repeat the default arguments.",
            paramPos)
        js.Undefined()
      }
    } else {
      genApplyMethod(targetTree, defaultGetter, defaultGetterArgs)
    }
  }

  private def targetSymForDefaultGetter(sym: Symbol): Symbol = {
    if (sym.isClassConstructor) {
      /*/* Get the companion module class.
       * For inner classes the sym.owner.companionModule can be broken,
       * therefore companionModule is fetched at uncurryPhase.
       */
      val companionModule = enteringPhase(currentRun.namerPhase) {
        sym.owner.companionModule
      }
      companionModule.moduleClass*/
      sym.owner.companionModule.moduleClass
    } else {
      sym.owner
    }
  }

  private def defaultGetterDenot(targetSym: Symbol, sym: Symbol, paramIndex: Int): Denotation =
    targetSym.info.member(DefaultGetterName(sym.name.asTermName, paramIndex))

  private def defaultGetterDenot(sym: Symbol, paramIndex: Int): Denotation =
    defaultGetterDenot(targetSymForDefaultGetter(sym), sym, paramIndex)

  /** Generate the final forwarding call to the exported method. */
  private def genResult(exported: Exported, args: List[js.Tree], static: Boolean)(
      implicit pos: SourcePosition): js.Tree = {

    val sym = exported.sym

    def receiver = {
      if (static)
        genLoadModule(sym.owner)
      else if (sym.owner == defn.ObjectClass)
        js.This()(jstpe.ClassType(ir.Names.ObjectClass))
      else
        js.This()(encodeClassType(sym.owner))
    }

    def boxIfNeeded(call: js.Tree): js.Tree = {
      box(call, atPhase(elimErasedValueTypePhase)(sym.info.resultType))
    }

    if (currentClassSym.isNonNativeJSClass) {
      assert(sym.owner == currentClassSym.get, sym.fullName)
      boxIfNeeded(genApplyJSClassMethod(receiver, sym, args))
    } else {
      if (sym.isClassConstructor)
        js.New(encodeClassName(currentClassSym), encodeMethodSym(sym), args)
      else if (sym.isPrivate)
        boxIfNeeded(genApplyMethodStatically(receiver, sym, args))
      else
        boxIfNeeded(genApplyMethod(receiver, sym, args))
    }
  }

  private def genThrowTypeError(msg: String = "No matching overload")(implicit pos: Position): js.Tree =
    js.Throw(js.JSNew(js.JSGlobalRef("TypeError"), js.StringLiteral(msg) :: Nil))

  private final class ParamSpec(val sym: Symbol, val info: Type,
      val isRepeated: Boolean, val hasDefault: Boolean) {
    override def toString(): String =
      s"ParamSpec(${sym.name}, ${info.show}, isRepeated = $isRepeated, hasDefault = $hasDefault)"
  }

  private object ParamSpec {
    def apply(methodSym: Symbol, sym: Symbol, infoAtElimRepeated: Type, infoAtElimEVT: Type,
        methodHasDefaultParams: Boolean, paramIndex: Int): ParamSpec = {
      val isRepeated = infoAtElimRepeated.isRepeatedParam
      val info = if (isRepeated) infoAtElimRepeated.repeatedToSingle else infoAtElimEVT
      val hasDefault = methodHasDefaultParams && defaultGetterDenot(methodSym, paramIndex).exists
      new ParamSpec(sym, info, isRepeated, hasDefault)
    }
  }

  // This is a case class because we rely on its structural equality
  private final case class Exported(sym: Symbol) {
    val isConstructorOfNestedJSClass =
      sym.isClassConstructor && sym.owner.isNestedJSClass

    // params: List[ParamSpec] ; captureParams and captureParamsBack: List[js.ParamDef]
    val (params, captureParamsFront, captureParamsBack) = {
      val paramNamessNow = sym.info.paramNamess
      val paramInfosNow = sym.info.paramInfoss.flatten
      val paramSymsAtElimRepeated = atPhase(elimRepeatedPhase)(sym.paramSymss.flatten.filter(_.isTerm))
      val (paramNamessAtElimRepeated, paramInfosAtElimRepeated, methodHasDefaultParams) =
        atPhase(elimRepeatedPhase)((sym.info.paramNamess, sym.info.paramInfoss.flatten, sym.hasDefaultParams))
      val (paramNamessAtElimEVT, paramInfosAtElimEVT) =
        atPhase(elimErasedValueTypePhase)((sym.info.paramNamess, sym.info.paramInfoss.flatten))

      def buildFormalParams(paramSyms: List[Symbol], paramInfosAtElimRepeated: List[Type],
          paramInfosAtElimEVT: List[Type]): IndexedSeq[ParamSpec] = {
        (for {
          (paramSym, infoAtElimRepeated, infoAtElimEVT, paramIndex) <-
            paramSyms.lazyZip(paramInfosAtElimRepeated).lazyZip(paramInfosAtElimEVT).lazyZip(0 until paramSyms.size)
        } yield {
          ParamSpec(sym, paramSym, infoAtElimRepeated, infoAtElimEVT, methodHasDefaultParams, paramIndex)
        }).toIndexedSeq
      }

      if (!isConstructorOfNestedJSClass) {
        // Easy case: all params are formal params.
        assert(paramInfosAtElimRepeated.size == paramInfosAtElimEVT.size,
            s"Found ${paramInfosAtElimRepeated.size} params entering elimRepeated but " +
            s"${paramInfosAtElimEVT.size} params entering elimErasedValueType for " +
            s"non-lifted symbol ${sym.fullName}")
        val formalParams = buildFormalParams(paramSymsAtElimRepeated, paramInfosAtElimRepeated, paramInfosAtElimEVT)
        (formalParams, Nil, Nil)
      } else {
        /* The `arg$outer` param is added by erasure, following "instructions"
         * by explicitouter, while the other capture params are added by
         * lambdalift (between elimErasedValueType and now).
         *
         * Since we cannot reliably get Symbols for parameters created by
         * intermediate phases, we have to perform some dance with the
         * paramNamess and paramInfoss observed at some phases, combined with
         * the paramSymss observed at elimRepeated.
         */

        val hasOuterParam = {
          paramInfosAtElimEVT.size == paramInfosAtElimRepeated.size + 1 &&
          paramNamessAtElimEVT.flatten.head == nme.OUTER
        }
        assert(
            hasOuterParam || paramInfosAtElimEVT.size == paramInfosAtElimRepeated.size,
            s"Found ${paramInfosAtElimRepeated.size} params entering elimRepeated but " +
            s"${paramInfosAtElimEVT.size} params entering elimErasedValueType for " +
            s"lifted constructor symbol ${sym.fullName}")

        val startOfFormalParams = paramNamessNow.flatten.indexOfSlice(paramNamessAtElimRepeated.flatten)
        val formalParamCount = paramInfosAtElimRepeated.size

        val nonOuterParamInfosAtElimEVT =
          if (hasOuterParam) paramInfosAtElimEVT.tail
          else paramInfosAtElimEVT
        val formalParams = buildFormalParams(paramSymsAtElimRepeated, paramInfosAtElimRepeated, nonOuterParamInfosAtElimEVT)

        val paramNamesAndInfosNow = paramNamessNow.flatten.zip(paramInfosNow)
        val (captureParamsFrontNow, restOfParamsNow) = paramNamesAndInfosNow.splitAt(startOfFormalParams)
        val captureParamsBackNow = restOfParamsNow.drop(formalParamCount)

        def makeCaptureParamDef(nameAndInfo: (TermName, Type)): js.ParamDef = {
          implicit val pos: Position = sym.span
          js.ParamDef(freshLocalIdent(nameAndInfo._1.mangledString), NoOriginalName, toIRType(nameAndInfo._2),
              mutable = false, rest = false)
        }

        val captureParamsFront = captureParamsFrontNow.map(makeCaptureParamDef(_))
        val captureParamsBack = captureParamsBackNow.map(makeCaptureParamDef(_))

        (formalParams, captureParamsFront, captureParamsBack)
      }
    }

    val hasRepeatedParam = params.nonEmpty && params.last.isRepeated

    def pos: SourcePosition = sym.sourcePos

    def exportArgTypeAt(paramIndex: Int): Type = {
      if (paramIndex < params.length) {
        params(paramIndex).info
      } else {
        assert(hasRepeatedParam, i"$sym does not have varargs nor enough params for $paramIndex")
        params.last.info
      }
    }

    def typeInfo: String = sym.info.toString
  }

  private sealed abstract class RTTypeTest

  private case class PrimitiveTypeTest(tpe: jstpe.Type, rank: Int) extends RTTypeTest

  private case class InstanceOfTypeTest(tpe: Type) extends RTTypeTest {
    override def equals(that: Any): Boolean = {
      that match {
        case InstanceOfTypeTest(thatTpe) => tpe =:= thatTpe
        case _                           => false
      }
    }
  }

  private case object NoTypeTest extends RTTypeTest

  private object RTTypeTest {
    given PartialOrdering[RTTypeTest] {
      override def tryCompare(lhs: RTTypeTest, rhs: RTTypeTest): Option[Int] = {
        if (lteq(lhs, rhs)) if (lteq(rhs, lhs)) Some(0) else Some(-1)
        else                if (lteq(rhs, lhs)) Some(1) else None
      }

      override def lteq(lhs: RTTypeTest, rhs: RTTypeTest): Boolean = {
        (lhs, rhs) match {
          // NoTypeTest is always last
          case (_, NoTypeTest) => true
          case (NoTypeTest, _) => false

          case (PrimitiveTypeTest(_, rank1), PrimitiveTypeTest(_, rank2)) =>
            rank1 <= rank2

          case (InstanceOfTypeTest(t1), InstanceOfTypeTest(t2)) =>
            t1 <:< t2

          case (_: PrimitiveTypeTest, _: InstanceOfTypeTest) => true
          case (_: InstanceOfTypeTest, _: PrimitiveTypeTest) => false
        }
      }

      override def equiv(lhs: RTTypeTest, rhs: RTTypeTest): Boolean = {
        lhs == rhs
      }
    }
  }

  /** Very simple O(n²) topological sort for elements assumed to be distinct. */
  private def topoSortDistinctsBy[A <: AnyRef, B](coll: List[A])(f: A => B)(
      using ord: PartialOrdering[B]): List[A] = {

    @tailrec
    def loop(coll: List[A], acc: List[A]): List[A] = {
      if (coll.isEmpty) acc
      else if (coll.tail.isEmpty) coll.head :: acc
      else {
        val (lhs, rhs) = coll.span(x => !coll.forall(y => (x eq y) || !ord.lteq(f(x), f(y))))
        assert(!rhs.isEmpty, s"cycle while ordering $coll")
        loop(lhs ::: rhs.tail, rhs.head :: acc)
      }
    }

    loop(coll, Nil)
  }

  private def typeTestForTpe(tpe: Type): RTTypeTest = {
    tpe match {
      case tpe: ErasedValueType =>
        InstanceOfTypeTest(tpe.tycon.typeSymbol.typeRef)

      case _ =>
        import org.scalajs.ir.Names

        (toIRType(tpe): @unchecked) match {
          case jstpe.AnyType => NoTypeTest

          case jstpe.NoType      => PrimitiveTypeTest(jstpe.UndefType, 0)
          case jstpe.BooleanType => PrimitiveTypeTest(jstpe.BooleanType, 1)
          case jstpe.CharType    => PrimitiveTypeTest(jstpe.CharType, 2)
          case jstpe.ByteType    => PrimitiveTypeTest(jstpe.ByteType, 3)
          case jstpe.ShortType   => PrimitiveTypeTest(jstpe.ShortType, 4)
          case jstpe.IntType     => PrimitiveTypeTest(jstpe.IntType, 5)
          case jstpe.LongType    => PrimitiveTypeTest(jstpe.LongType, 6)
          case jstpe.FloatType   => PrimitiveTypeTest(jstpe.FloatType, 7)
          case jstpe.DoubleType  => PrimitiveTypeTest(jstpe.DoubleType, 8)

          case jstpe.ClassType(Names.BoxedUnitClass)   => PrimitiveTypeTest(jstpe.UndefType, 0)
          case jstpe.ClassType(Names.BoxedStringClass) => PrimitiveTypeTest(jstpe.StringType, 9)
          case jstpe.ClassType(_)                      => InstanceOfTypeTest(tpe)

          case jstpe.ArrayType(_) => InstanceOfTypeTest(tpe)
        }
    }
  }

  // Group-by that does not rely on hashCode(), only equals() - O(n²)
  private def groupByWithoutHashCode[A, B](coll: List[A])(f: A => B): List[(B, List[A])] = {
    val m = new mutable.ArrayBuffer[(B, List[A])]
    m.sizeHint(coll.length)

    for (elem <- coll) {
      val key = f(elem)
      val index = m.indexWhere(_._1 == key)
      if (index < 0)
        m += ((key, List(elem)))
      else
        m(index) = (key, elem :: m(index)._2)
    }

    m.toList
  }

  private class FormalArgsRegistry(minArgc: Int, needsRestParam: Boolean) {
    private val fixedParamNames: scala.collection.immutable.IndexedSeq[jsNames.LocalName] =
      (0 until minArgc).toIndexedSeq.map(_ => freshLocalIdent("arg")(NoPosition).name)

    private val restParamName: jsNames.LocalName =
      if (needsRestParam) freshLocalIdent("rest")(NoPosition).name
      else null

    def genFormalArgs()(implicit pos: Position): List[js.ParamDef] = {
      val fixedParamDefs = fixedParamNames.toList.map { paramName =>
        js.ParamDef(js.LocalIdent(paramName), NoOriginalName, jstpe.AnyType, mutable = false, rest = false)
      }

      if (needsRestParam) {
        val restParamDef =
          js.ParamDef(js.LocalIdent(restParamName), NoOriginalName, jstpe.AnyType, mutable = false, rest = true)
        fixedParamDefs :+ restParamDef
      } else {
        fixedParamDefs
      }
    }

    def genArgRef(index: Int)(implicit pos: Position): js.Tree = {
      if (index < minArgc)
        js.VarRef(js.LocalIdent(fixedParamNames(index)))(jstpe.AnyType)
      else
        js.JSSelect(genRestArgRef(), js.IntLiteral(index - minArgc))
    }

    def genVarargRef(fixedParamCount: Int)(implicit pos: Position): js.Tree = {
      assert(fixedParamCount >= minArgc, s"genVarargRef($fixedParamCount) with minArgc = $minArgc at $pos")
      val restParam = genRestArgRef()
      if (fixedParamCount == minArgc)
        restParam
      else
        js.JSMethodApply(restParam, js.StringLiteral("slice"), List(js.IntLiteral(fixedParamCount - minArgc)))
    }

    def genRestArgRef()(implicit pos: Position): js.Tree = {
      assert(needsRestParam, s"trying to generate a reference to non-existent rest param at $pos")
      js.VarRef(js.LocalIdent(restParamName))(jstpe.AnyType)
    }

    def genAllArgsRefsForForwarder()(implicit pos: Position): List[js.Tree] = {
      val fixedArgRefs = fixedParamNames.toList.map { paramName =>
        js.VarRef(js.LocalIdent(paramName))(jstpe.AnyType)
      }

      if (needsRestParam) {
        val restArgRef = js.VarRef(js.LocalIdent(restParamName))(jstpe.AnyType)
        fixedArgRefs :+ restArgRef
      } else {
        fixedArgRefs
      }
    }
  }
}
