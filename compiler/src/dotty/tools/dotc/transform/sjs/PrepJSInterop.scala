package dotty.tools.dotc
package transform
package sjs

import scala.collection.mutable

import ast.{Trees, tpd, untpd}
import core._
import reporting._
import typer.Checking
import util.SrcPos
import Annotations._
import Constants._
import Contexts._
import Decorators._
import DenotTransformers._
import Flags._
import NameKinds.DefaultGetterName
import NameOps._
import Names._
import Phases._
import Scopes._
import StdNames._
import Symbols._
import SymDenotations._
import SymUtils._
import Trees._
import Types._

import JSInteropUtils._

import org.scalajs.ir.Trees.{JSGlobalRef, JSNativeLoadSpec}

import dotty.tools.dotc.config.SJSPlatform.sjsPlatform
import dotty.tools.backend.sjs.JSDefinitions.jsdefn

/** A macro transform that runs after typer and before pickler to perform
 *  additional Scala.js-specific checks and transformations necessary for
 *  interoperability with JavaScript.
 *
 *  It performs the following functions:
 *
 *  - Sanity checks for the js.Any hierarchy
 *  - Annotate subclasses of js.Any to be treated specially
 *  - Create JSExport methods: Dummy methods that are propagated
 *    through the whole compiler chain to mark exports. This allows
 *    exports to have the same semantics than methods.
 *
 *  This is the equivalent of `PrepJSInterop` in Scala 2, minus the handling
 *  of `scala.Enumeration`.
 *
 *  The reason for making this a macro transform is that some functions (in particular
 *  all the checks that behave differently depending on properties of classes in
 *  the enclosing class chain) are naturally top-down and don't lend themselves to the
 *  bottom-up approach of a mini phase.
 *
 *  In addition, the addition of export forwarders must be done before pickling to
 *  be signature-compatible with scalac, and there are only macro transforms before
 *  pickling.
 */
class PrepJSInterop extends MacroTransform with IdentityDenotTransformer { thisPhase =>
  import PrepJSInterop._
  import tpd._

  override def phaseName: String = PrepJSInterop.name

  override def isEnabled(using Context): Boolean =
    ctx.settings.scalajs.value

  override def changesMembers: Boolean = true // the phase adds export forwarders

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    sjsPlatform.perRunInfo.clear()
    super.runOn(units)
  }

  protected def newTransformer(using Context): Transformer =
    new ScalaJSPrepJSInteropTransformer

  class ScalaJSPrepJSInteropTransformer extends Transformer with Checking {
    import PrepJSExports._

    /** Kind of the directly enclosing (most nested) owner. */
    private var enclosingOwner: OwnerKind = OwnerKind.None

    /** Cumulative kinds of all enclosing owners. */
    private var allEnclosingOwners: OwnerKind = OwnerKind.None

    /** Nicer syntax for `allEnclosingOwners is kind`. */
    private def anyEnclosingOwner: OwnerKind = allEnclosingOwners

    /** Nicer syntax for `allEnclosingOwners isnt kind`. */
    private object noEnclosingOwner {
      @inline def is(kind: OwnerKind): Boolean =
        allEnclosingOwners isnt kind
    }

    private def enterOwner[A](kind: OwnerKind)(body: => A): A = {
      require(kind.isBaseKind, kind)
      val oldEnclosingOwner = enclosingOwner
      val oldAllEnclosingOwners = allEnclosingOwners
      enclosingOwner = kind
      allEnclosingOwners |= kind
      try {
        body
      } finally {
        enclosingOwner = oldEnclosingOwner
        allEnclosingOwners = oldAllEnclosingOwners
      }
    }

    /** Whether to check that we have proper literals in some crucial places.
     *
     *  This is always true in dotc. We keep the definition so that the code
     *  code can be as similar as possible to the scalac phase.
     */
    private final val shouldCheckLiterals = true

    /** Whether to check and prepare exports.
     *
     *  This is always true in dotc. We keep the definition so that the code
     *  code can be as similar as possible to the scalac phase.
     */
    private final val shouldPrepareExports = true

    /** DefDefs in class templates that export methods to JavaScript */
    private val exporters = mutable.Map.empty[Symbol, mutable.ListBuffer[Tree]]

    override def transform(tree: Tree)(using Context): Tree = {
      tree match {
        case tree: ValDef if tree.symbol.is(Module) =>
          /* Never apply this transformation on the term definition of modules.
           * Instead, all relevant checks are performed on the module class definition.
           */
          super.transform(tree)

        case tree: MemberDef => transformMemberDef(tree)
        case tree: Template  => transformTemplate(tree)
        case _               => transformStatOrExpr(tree)
      }
    }

    private def transformMemberDef(tree: MemberDef)(using Context): Tree = {
      val sym = tree.symbol

      checkInternalAnnotations(sym)

      /* Checks related to @js.native:
       * - if @js.native, verify that it is allowed in this context, and if
       *   yes, compute and store the JS native load spec
       * - if not @js.native, verify that we do not use any other annotation
       *   reserved for @js.native members (namely, JS native load spec annots)
       */
      val isJSNative = sym.getAnnotation(jsdefn.JSNativeAnnot) match {
        case Some(annot) =>
          checkJSNativeDefinition(tree, annot.tree, sym)
          true
        case None =>
          checkJSNativeSpecificAnnotsOnNonJSNative(tree)
          false
      }

      checkJSNameAnnots(sym)

      val transformedTree: Tree = tree match {
        case tree: TypeDef if tree.isClassDef =>
          if (shouldPrepareExports)
            registerClassOrModuleExports(sym)

          if (isJSAny(sym))
            transformJSClassDef(tree)
          else
            transformScalaClassDef(tree)

        case _: TypeDef =>
          super.transform(tree)

        case tree: ValOrDefDef =>
          /* Prepare exports for methods, local defs and local variables.
           * Avoid *fields* (non-local non-method) because they all have a
           * corresponding getter, which is the one that handles exports.
           * (Note that local-to-block can never have exports, but the error
           * messages for that are handled by genExportMember).
           */
          if (shouldPrepareExports && (sym.is(Method) || sym.isLocalToBlock)) {
            exporters.getOrElseUpdate(sym.owner, mutable.ListBuffer.empty) ++=
              genExportMember(sym)
          }

          if (sym.isLocalToBlock) {
            super.transform(tree)
          } else if (isJSNative) {
            transformJSNativeValOrDefDef(tree)
          } else if (enclosingOwner is OwnerKind.JSType) {
            transformValOrDefDefInJSType(tree)
          } else {
            transformScalaValOrDefDef(tree)
          }
      }

      /* Give tree.symbol, not sym, so that for modules it is the module
       * symbol, not the module class symbol.
       *
       * #1899 This must be done *after* transforming the member def tree,
       * because fixPublicBeforeTyper must have run.
       */
      markExposedIfRequired(tree.symbol)

      transformedTree
    }

    private def transformScalaClassDef(tree: TypeDef)(using Context): Tree = {
      val sym = tree.symbol

      // In native JS things, only js.Any stuff is allowed
      if (enclosingOwner is OwnerKind.JSNative) {
        /* We have to allow synthetic companion objects here, as they get
         * generated when a nested native JS class has default arguments in
         * its constructor (see #1891).
         */
        if (!sym.is(Synthetic)) {
          report.error(
              "Native JS traits, classes and objects cannot contain inner " +
              "Scala traits, classes or objects (i.e., not extending js.Any)",
              tree)
        }
      }

      if (sym == jsdefn.PseudoUnionClass)
        sym.addAnnotation(jsdefn.JSTypeAnnot)

      val kind =
        if (sym.is(Module)) OwnerKind.ScalaMod
        else OwnerKind.ScalaClass
      enterOwner(kind) {
        super.transform(tree)
      }
    }

    private def transformScalaValOrDefDef(tree: ValOrDefDef)(using Context): Tree = {
      // There is nothing special to do for a Scala val or def
      super.transform(tree)
    }

    private def transformTemplate(tree: Template)(using Context): Template = {
      // First, recursively transform the template
      val transformedTree = super.transform(tree).asInstanceOf[Template]

      val clsSym = ctx.owner

      // Check that @JSExportStatic fields come first
      if (clsSym.is(ModuleClass)) { // quick check to avoid useless work
        var foundStatOrNonStaticVal: Boolean = false
        for (tree <- transformedTree.body) {
          tree match {
            case vd: ValDef if vd.symbol.hasAnnotation(jsdefn.JSExportStaticAnnot) =>
              if (foundStatOrNonStaticVal) {
                report.error(
                    "@JSExportStatic vals and vars must be defined before " +
                    "any other val/var, and before any constructor " +
                    "statement.",
                    vd)
              }
            case vd: ValDef if !vd.symbol.is(Lazy) =>
              foundStatOrNonStaticVal = true
            case _: MemberDef =>
            case _ =>
              foundStatOrNonStaticVal = true
          }
        }
      }

      // Add exports to the template, if there are any
      exporters.get(clsSym).fold {
        transformedTree
      } { exports =>
        cpy.Template(transformedTree)(
          transformedTree.constr,
          transformedTree.parents,
          Nil,
          transformedTree.self,
          transformedTree.body ::: exports.toList
        )
      }
    }

    private def transformStatOrExpr(tree: Tree)(using Context): Tree = {
      tree match {
        /* This might not be needed in dotty.
        /* Anonymous function, need to check that it is not used as a SAM for a
         * JS type, unless it is js.FunctionN or js.ThisFunctionN.
         * See #2921.
         */
        case tree: Function =>
          val tpeSym = tree.tpe.typeSymbol
          if (isJSAny(tpeSym) && !AllJSFunctionClasses.contains(tpeSym)) {
            report.error(
                "Using an anonymous function as a SAM for the JavaScript " +
                "type " + tpeSym.fullNameString + " is not allowed. " +
                "Use an anonymous class instead.",
                tree)
          }
          super.transform(tree)
        */

        // Validate js.constructorOf[T]
        case TypeApply(ctorOfTree, List(tpeArg))
            if ctorOfTree.symbol == jsdefn.JSPackage_constructorOf =>
          validateJSConstructorOf(tree, tpeArg)
          super.transform(tree)

        /* Rewrite js.ConstructorTag.materialize[T] into
         * runtime.newConstructorTag[T](js.constructorOf[T])
         */
        case TypeApply(ctorOfTree, List(tpeArg))
            if ctorOfTree.symbol == jsdefn.JSConstructorTag_materialize =>
          validateJSConstructorOf(tree, tpeArg)
          val ctorOf = ref(jsdefn.JSPackage_constructorOf).appliedToTypeTree(tpeArg)
          ref(jsdefn.Runtime_newConstructorTag).appliedToType(tpeArg.tpe).appliedTo(ctorOf)

        // Compile-time errors and warnings for js.Dynamic.literal
        case Apply(Apply(fun, nameArgs), args)
            if fun.symbol == jsdefn.JSDynamicLiteral_applyDynamic ||
              fun.symbol == jsdefn.JSDynamicLiteral_applyDynamicNamed =>
          // Check that the first argument list is a constant string "apply"
          nameArgs match {
            case List(Literal(Constant(s: String))) =>
              if (s != "apply")
                report.error(i"js.Dynamic.literal does not have a method named $s", tree)
            case _ =>
              report.error(i"js.Dynamic.literal.${tree.symbol.name} may not be called directly", tree)
          }

          // TODO Warn for known duplicate property names

          super.transform(tree)

        case _ =>
          super.transform(tree)
      }
    }

    private def validateJSConstructorOf(tree: Tree, tpeArg: Tree)(using Context): Unit = {
      val tpe = checkClassType(tpeArg.tpe, tpeArg.srcPos, traitReq = false, stablePrefixReq = false)

      tpe.underlyingClassRef(refinementOK = false) match {
        case typeRef: TypeRef if typeRef.symbol.isOneOf(Trait | ModuleClass) =>
          report.error(i"non-trait class type required but $tpe found", tpeArg)
        case _ =>
          // an error was already reported above
      }
    }

    /** Performs checks and rewrites specific to classes / objects extending
     *  js.Any.
     */
    private def transformJSClassDef(classDef: TypeDef)(using Context): Tree = {
      val sym = classDef.symbol

      sym.addAnnotation(jsdefn.JSTypeAnnot)

      /*val isJSLambda =
        sym.isAnonymousClass && AllJSFunctionClasses.exists(sym.isSubClass(_))
      if (isJSLambda)
        transformJSLambdaClassDef(classDef)
      else*/
      transformNonLambdaJSClassDef(classDef)
    }

    /*
    /** Performs checks and rewrites specific to JS lambdas, i.e., anonymous
     *  classes extending one of the JS function types.
     *
     *  JS lambdas are special because they are completely hijacked by the
     *  back-end, so although at this phase they look like normal anonymous
     *  classes, they do not behave like ones.
     */
    private def transformJSLambdaClassDef(classDef: TypeDef)(using Context): Tree = {
      /* For the purposes of checking inner members, a JS lambda acts as a JS
       * native class owner.
       *
       * TODO This is probably not right, but historically it has always been
       * that way. It should be revisited.
       */
      enterOwner(OwnerKind.JSNativeClass) {
        super.transform(classDef)
      }
    }
    */

    /** Performs checks and rewrites for all JS classes, traits and objects
     *  except JS lambdas.
     */
    private def transformNonLambdaJSClassDef(classDef: TypeDef)(using Context): Tree = {
      val sym = classDef.symbol
      val isJSNative = sym.hasAnnotation(jsdefn.JSNativeAnnot)

      // Forbid @EnableReflectiveInstantiation on JS types
      sym.getAnnotation(jsdefn.EnableReflectiveInstantiationAnnot).foreach { annot =>
        report.error(
            "@EnableReflectiveInstantiation cannot be used on types extending js.Any.",
            annot.tree)
      }

      // Forbid package objects that extends js.Any
      if (sym.isPackageObject)
        report.error("Package objects may not extend js.Any.", classDef)

      // Check that we do not have a case modifier
      if (sym.is(Case)) {
        report.error(
            "Classes and objects extending js.Any may not have a case modifier",
            classDef)
      }

      // Check the parents
      for (parent <- sym.info.parents) {
        parent.typeSymbol match {
          case parentSym if parentSym == defn.ObjectClass =>
            // AnyRef is valid, except for non-native JS classes and objects
            if (!isJSNative && !sym.is(Trait)) {
              report.error(
                  "Non-native JS classes and objects cannot directly extend AnyRef. They must extend a JS class (native or not).",
                  classDef)
            }
          case parentSym if isJSAny(parentSym) =>
            // A non-native JS type cannot extend a native JS trait
            // Otherwise, extending a JS type is valid
            if (!isJSNative && parentSym.is(Trait) && parentSym.hasAnnotation(jsdefn.JSNativeAnnot)) {
              report.error(
                  "Non-native JS types cannot directly extend native JS traits.",
                  classDef)
            }
          case parentSym if parentSym == defn.DynamicClass =>
            /* We have to allow scala.Dynamic to be able to define js.Dynamic
             * and similar constructs.
             * This causes the unsoundness filed as #1385.
             */
          case parentSym =>
            /* This is a Scala class or trait other than AnyRef and Dynamic,
             * which is never valid.
             */
            report.error(
                i"${sym.name} extends ${parentSym.fullName} which does not extend js.Any.",
                classDef)
        }
      }

      // Checks for non-native JS stuff
      if (!isJSNative) {
        // It cannot be in a native JS class or trait
        if (enclosingOwner is OwnerKind.JSNativeClass) {
          report.error(
              "Native JS classes and traits cannot contain non-native JS classes, traits or objects",
              classDef)
        }

        // Unless it is a trait, it cannot be in a native JS object
        if (!sym.is(Trait) && (enclosingOwner is OwnerKind.JSNativeMod)) {
          report.error(
              "Native JS objects cannot contain inner non-native JS classes or objects",
              classDef)
        }

        // Local JS classes cannot be abstract (implementation restriction)
        if (sym.is(Abstract, butNot = Trait) && sym.isLocalToBlock) {
          report.error(
              "Implementation restriction: local JS classes cannot be abstract",
              classDef)
        }
      }

      // Check for consistency of JS semantics across overriding
      val overridingPairsCursor = new OverridingPairs.Cursor(sym)
      while (overridingPairsCursor.hasNext) {
        val overriding = overridingPairsCursor.overriding
        val overridden = overridingPairsCursor.overridden
        overridingPairsCursor.next() // prepare for next turn

        val clsSym = sym

        if (overriding.isTerm) {
          def errorPos = {
            if (clsSym == overriding.owner) overriding.srcPos
            else if (clsSym == overridden.owner) overridden.srcPos
            else clsSym.srcPos
          }

          // Some utils inspired by RefChecks

          def infoString0(sym: Symbol, showLocation: Boolean): String = {
            val sym1 = sym.underlyingSymbol
            def info = clsSym.thisType.memberInfo(sym1)
            val infoStr =
              if (sym1.is(Module)) ""
              else i" of type $info"
            i"${if (showLocation) sym1.showLocated else sym1}$infoStr with JS name '${sym.jsName.displayName}'"
          }

          def infoString(sym: Symbol): String = infoString0(sym, sym.owner != clsSym)
          def infoStringWithLocation(sym: Symbol): String = infoString0(sym, true)

          def emitOverrideError(msg: String): Unit = {
            report.error(
              "error overriding %s;\n  %s %s".format(
                infoStringWithLocation(overridden), infoString(overriding), msg),
              errorPos)
          }

          // Check for overrides with different JS names - issue #1983
          if (overriding.jsName != overridden.jsName)
            emitOverrideError("has a different JS name")

          /* Cannot override a non-@JSOptional with an @JSOptional. Unfortunately
           * at this point the symbols do not have @JSOptional yet, so we need
           * to detect whether it would be applied.
           */
          if (!isJSNative) {
            def isJSOptional(sym: Symbol): Boolean = {
              sym.owner.is(Trait) && !sym.is(Deferred) && !sym.isConstructor &&
              !sym.owner.hasAnnotation(jsdefn.JSNativeAnnot)
            }
            if (isJSOptional(overriding) && !(overridden.is(Deferred) || isJSOptional(overridden)))
              emitOverrideError("cannot override a concrete member in a non-native JS trait")
          }
        }
      }

      val kind = {
        if (!isJSNative) {
          if (sym.is(ModuleClass)) OwnerKind.JSMod
          else OwnerKind.JSClass
        } else {
          if (sym.is(ModuleClass)) OwnerKind.JSNativeMod
          else OwnerKind.JSNativeClass
        }
      }
      enterOwner(kind) {
        super.transform(classDef)
      }
    }

    private def checkJSNativeDefinition(treePos: SrcPos, annotPos: SrcPos, sym: Symbol)(using Context): Unit = {
      // Check if we may have a JS native here
      if (sym.isLocalToBlock) {
        report.error("@js.native is not allowed on local definitions", annotPos)
      } else if (!sym.isClass && (anyEnclosingOwner is (OwnerKind.ScalaClass | OwnerKind.JSType))) {
        report.error("@js.native vals and defs can only appear in static Scala objects", annotPos)
      } else if (sym.isClass && !isJSAny(sym)) {
        report.error("Classes, traits and objects not extending js.Any may not have an @js.native annotation", annotPos)
      } else if (anyEnclosingOwner is OwnerKind.ScalaClass) {
        report.error("Scala traits and classes may not have native JS members", annotPos)
      } else if (enclosingOwner is OwnerKind.JSNonNative) {
        report.error("non-native JS classes, traits and objects may not have native JS members", annotPos)
      } else {
        // The symbol can be annotated with @js.native. Now check its JS native loading spec.
        if (sym.is(Trait)) {
          assert(sym.is(Trait), sym) // just tested in the previous `if`
          for (annot <- sym.annotations) {
            val annotSym = annot.symbol
            if (isJSNativeLoadingSpecAnnot(annotSym))
              report.error(i"Traits may not have an @${annotSym.name} annotation.", annot.tree)
          }
        } else {
          /* Compute the loading spec now, before `flatten` destroys the name.
           * We store it in a global map.
           */
          val optLoadSpec = checkAndComputeJSNativeLoadSpecOf(treePos, sym)
          for (loadSpec <- optLoadSpec)
            sjsPlatform.perRunInfo.storeJSNativeLoadSpec(sym, loadSpec)
        }
      }
    }

    private def checkAndComputeJSNativeLoadSpecOf(pos: SrcPos, sym: Symbol)(
        using Context): Option[JSNativeLoadSpec] = {
      import JSNativeLoadSpec._

      def makeGlobalRefNativeLoadSpec(globalRef: String,
          path: List[String]): Global = {
        val validatedGlobalRef = if (!JSGlobalRef.isValidJSGlobalRefName(globalRef)) {
          report.error(s"The name of a JS global variable must be a valid JS identifier (got '$globalRef')", pos)
          "erroneous"
        } else {
          globalRef
        }
        Global(validatedGlobalRef, path)
      }

      if (enclosingOwner is OwnerKind.JSNative) {
        /* We cannot get here for @js.native vals and defs. That would mean we
         * have an @js.native val/def inside a JavaScript type, which is not
         * allowed and already caught in checkJSNativeDefinition().
         */
        assert(sym.isClass,
            s"undetected @js.native val or def ${sym.fullName} inside JS type at $pos")

        for (annot <- sym.annotations) {
          val annotSym = annot.symbol
          if (isJSNativeLoadingSpecAnnot(annotSym))
            report.error(i"Nested JS classes and objects cannot have an @${annotSym.name} annotation.", annot.tree)
        }

        if (sym.owner.isStaticOwner) {
          for (annot <- sym.annotations) {
            if (annot.symbol == jsdefn.JSNameAnnot && !(annot.arguments.head.tpe <:< defn.StringType)) {
              report.error(
                  "Implementation restriction: " +
                  "@JSName with a js.Symbol is not supported on nested native classes and objects",
                  annot.tree)
            }
          }

          val jsName = sym.jsName match {
            case JSName.Literal(jsName) => jsName
            case JSName.Computed(_)     => "<erroneous>" // compile error above
          }

          val ownerLoadSpec = sjsPlatform.perRunInfo.jsNativeLoadSpecOfOption(sym.owner)
          val loadSpec = ownerLoadSpec match {
            case None =>
              // The owner is a JSGlobalScope
              makeGlobalRefNativeLoadSpec(jsName, Nil)
            case Some(Global(globalRef, path)) =>
              Global(globalRef, path :+ jsName)
            case Some(Import(module, path)) =>
              Import(module, path :+ jsName)
            case Some(ImportWithGlobalFallback(
                Import(module, modulePath), Global(globalRef, globalPath))) =>
              ImportWithGlobalFallback(
                  Import(module, modulePath :+ jsName),
                  Global(globalRef, globalPath :+ jsName))
          }
          Some(loadSpec)
        } else {
          None
        }
      } else {
        def parsePath(pathName: String): List[String] =
          pathName.split('.').toList

        def parseGlobalPath(pathName: String): Global = {
          val globalRef :: path = parsePath(pathName)
          makeGlobalRefNativeLoadSpec(globalRef, path)
        }

        checkAndGetJSNativeLoadingSpecAnnotOf(pos, sym) match {
          case Some(annot) if annot.symbol == jsdefn.JSGlobalScopeAnnot =>
            if (!sym.is(Module)) {
              report.error(
                  "@JSGlobalScope can only be used on native JS objects (with @js.native).",
                  annot.tree)
            }
            None

          case Some(annot) if annot.symbol == jsdefn.JSGlobalAnnot =>
            if (shouldCheckLiterals)
              checkJSGlobalLiteral(annot)
            val pathName = annot.argumentConstantString(0).getOrElse {
              val needsExplicitJSName = {
                (enclosingOwner is OwnerKind.ScalaMod) &&
                !sym.owner.isPackageObject
              }

              if (needsExplicitJSName) {
                report.error(
                    "Native JS members inside non-native objects must have an explicit name in @JSGlobal",
                    annot.tree)
              }
              sym.defaultJSName
            }
            Some(parseGlobalPath(pathName))

          case Some(annot) if annot.symbol == jsdefn.JSImportAnnot =>
            if (shouldCheckLiterals)
              checkJSImportLiteral(annot)
            val module = annot.argumentConstantString(0).getOrElse {
              "" // an error is reported by checkJSImportLiteral in this case
            }
            val path = annot.argumentConstantString(1).fold[List[String]](Nil)(parsePath)
            val importSpec = Import(module, path)
            val loadSpec = annot.argumentConstantString(2).fold[JSNativeLoadSpec] {
              importSpec
            } { globalPathName =>
              ImportWithGlobalFallback(importSpec, parseGlobalPath(globalPathName))
            }
            Some(loadSpec)

          case _ =>
            /* We already emitted an error. Invent something not to cause
             * cascading errors.
             */
            Some(JSNativeLoadSpec.Global("erroneous", Nil))
        }
      }
    }

    /** Verify a ValOrDefDef that is annotated with `@js.native`. */
    private def transformJSNativeValOrDefDef(tree: ValOrDefDef)(using Context): ValOrDefDef = {
      val sym = tree.symbol

      def annotPos(annotSym: Symbol): SrcPos =
        sym.getAnnotation(annotSym).get.tree

      if (sym.is(Lazy) || sym.isJSSetter)
        report.error("@js.native is not allowed on vars, lazy vals and setter defs", annotPos(jsdefn.JSNativeAnnot))
      else if (sym.isJSBracketAccess)
        report.error("@JSBracketAccess is not allowed on @js.native vals and defs", annotPos(jsdefn.JSBracketAccessAnnot))
      else if (sym.isJSBracketCall)
        report.error("@JSBracketCall is not allowed on @js.native vals and defs", annotPos(jsdefn.JSBracketCallAnnot))

      //if (!sym.is(Accessor))
      checkRHSCallsJSNative(tree, "@js.native members")

      // Check that we do not override or implement anything from a superclass
      val overriddenSymbols = sym.allOverriddenSymbols
      if (overriddenSymbols.hasNext) {
        val overridden = overriddenSymbols.next()
        val verb = if (overridden.is(Deferred)) "implement" else "override"
        report.error(i"An @js.native member cannot $verb the inherited member ${overridden.fullName}", tree)
      }

      tree
    }

    /** Verify a ValOrDefDef inside a js.Any */
    private def transformValOrDefDefInJSType(tree: ValOrDefDef)(using Context): Tree = {
      val sym = tree.symbol

      assert(!sym.isLocalToBlock, i"$tree at ${tree.span}")

      sym.name match {
        case nme.apply if !sym.hasAnnotation(jsdefn.JSNameAnnot) =>
          if (!sym.is(Method) || sym.isJSGetter) {
            report.error(
                "A member named apply represents function application in JavaScript. " +
                "A parameterless member should be exported as a property. " +
                "You must add @JSName(\"apply\")",
                sym)
          } else if (enclosingOwner is OwnerKind.JSNonNative) {
            report.error(
                "A non-native JS class cannot declare a method named `apply` without `@JSName`",
                sym)
          }

        case nme.equals_ if sym.info.matches(defn.Any_equals.info) =>
          report.warning(
              "Overriding equals in a JS class does not change how it is compared. " +
              "To silence this warning, change the name of the method and optionally add @JSName(\"equals\").",
              sym)

        case nme.hashCode_ if sym.info.matches(defn.Any_hashCode.info) =>
          report.warning(
              "Overriding hashCode in a JS class does not change its hash code. " +
              "To silence this warning, change the name of the method and optionally add @JSName(\"hashCode\").",
              sym)

        case _ =>
      }

      if (sym.isJSSetter)
        checkSetterSignature(sym, tree, exported = false)

      if (sym.isJSBracketAccess) {
        if (enclosingOwner is OwnerKind.JSNonNative) {
          report.error("@JSBracketAccess is not allowed in non-native JS classes", tree)
        } else {
          val allParamInfos = sym.info.paramInfoss.flatten

          allParamInfos.size match {
            case 1 =>
              // ok
            case 2 =>
              if (!sym.info.finalResultType.isRef(defn.UnitClass))
                report.error("@JSBracketAccess methods with two parameters must return Unit", tree)
            case _ =>
              report.error("@JSBracketAccess methods must have one or two parameters", tree)
          }

          if (allParamInfos.exists(_.isRepeatedParam))
            report.error("@JSBracketAccess methods may not have repeated parameters", tree)
          if (sym.hasDefaultParams)
            report.error("@JSBracketAccess methods may not have default parameters", tree)
        }
      }

      if (sym.isJSBracketCall) {
        if (enclosingOwner is OwnerKind.JSNonNative) {
          report.error("@JSBracketCall is not allowed in non-native JS classes", tree)
        } else {
          // JS bracket calls must have at least one non-repeated parameter
          sym.info.stripPoly match {
            case mt: MethodType if mt.paramInfos.nonEmpty && !mt.paramInfos.head.isRepeatedParam =>
              // ok
            case _ =>
              report.error("@JSBracketCall methods must have at least one non-repeated parameter", tree)
          }
        }
      }

      if (sym.hasAnnotation(defn.NativeAnnot)) {
        // Native methods are not allowed
        report.error("Methods in a js.Any may not be @native", tree)
      }

      /* In native JS types, there should not be any private member, except
       * private[this] constructors.
       */
      if ((enclosingOwner is OwnerKind.JSNative) && isPrivateMaybeWithin(sym)) {
        if (sym.isClassConstructor) {
          if (!sym.isAllOf(PrivateLocal)) {
            report.error(
                "Native JS classes may not have private constructors. " +
                "Use `private[this]` to declare an internal constructor.",
                sym)
          }
        } else if (!sym.is(ParamAccessor)) {
          report.error(
              "Native JS classes may not have private members. " +
              "Use a public member in a private facade instead.",
              tree)
        }
      }

      if (enclosingOwner is OwnerKind.JSNonNative) {
        // Private methods cannot be overloaded
        if (sym.is(Method) && isPrivateMaybeWithin(sym)) {
          val alts = sym.owner.info.memberBasedOnFlags(sym.name, required = Method)
          if (alts.isOverloaded) {
            report.error(
                "Private methods in non-native JS classes cannot be overloaded. Use different names instead.",
                tree)
          }
        }

        // private[Scope] methods must be final
        if (!sym.isOneOf(Final | Protected) && sym.privateWithin.exists && !sym.isClassConstructor)
          report.error("Qualified private members in non-native JS classes must be final", tree)

        // Traits must be pure interfaces, except for js.undefined members
        if (sym.owner.is(Trait) && sym.isTerm && !sym.isConstructor) {
          if (sym.is(Method) && isPrivateMaybeWithin(sym)) {
            report.error("A non-native JS trait cannot contain private members", tree)
          } else if (sym.is(Lazy)) {
            report.error("A non-native JS trait cannot contain lazy vals", tree)
          } else if (!sym.is(Deferred)) {
            /* Tell the back-end not to emit this thing. In fact, this only
             * matters for mixed-in members created from this member.
             */
            sym.addAnnotation(jsdefn.JSOptionalAnnot)

            if (!sym.isSetter) {
              // Check that methods do not have parens
              if (sym.is(Method, butNot = Accessor) && sym.info.stripPoly.isInstanceOf[MethodType])
                report.error("In non-native JS traits, defs with parentheses must be abstract.", tree.rhs)

              // Check that the rhs is `js.undefined`
              tree.rhs match {
                case sel: Select if sel.symbol == jsdefn.JSPackage_undefined =>
                  // ok
                case Apply(Apply(TypeApply(fromTypeConstructorFun, _), (sel: Select) :: Nil), _) if sel.symbol == jsdefn.JSPackage_undefined =>
                  // ok: js.|.fromTypeConstructor(js.undefined)(...)
                case _ =>
                  report.error(
                      "Members of non-native JS traits must either be abstract, or their right-hand-side must be `js.undefined`.",
                      tree)
              }
            }
          }
        }
      } else { // enclosingOwner isnt OwnerKind.JSNonNative
        // Check that the rhs is valid
        if (sym.isPrimaryConstructor || sym.isOneOf(Param | ParamAccessor | Deferred | Synthetic)
            || sym.name.is(DefaultGetterName) || sym.isSetter) {
          /* Ignore, i.e., allow:
           * - primary constructor
           * - all kinds of parameters
           * - setters
           * - default parameter getters (i.e., the default value of parameters)
           * - abstract members
           * - synthetic members (to avoid double errors with case classes, e.g. generated copy method)
           */
        } else if (sym.isConstructor) {
          // Force secondary ctor to have only a call to the primary ctor inside
          tree.rhs match {
            case Block(List(Apply(trg, _)), Literal(Constant(())))
                if trg.symbol.isPrimaryConstructor && trg.symbol.owner == sym.owner =>
              // everything is fine here
            case _ =>
              report.error(
                  "A secondary constructor of a native JS class may only call the primary constructor",
                  tree.rhs)
          }
        } else {
          // Check that the tree's rhs is exactly `= js.native`
          checkRHSCallsJSNative(tree, "Concrete members of JS native types")
        }
      }

      super.transform(tree)
    }

    private def checkRHSCallsJSNative(tree: ValOrDefDef, longKindStr: String)(using Context): Unit = {
      // Check that the rhs is exactly `= js.native`
      tree.rhs match {
        case sel: Select if sel.symbol == jsdefn.JSPackage_native =>
          // ok
        case _ =>
          val pos = if (tree.rhs != EmptyTree) tree.rhs.srcPos else tree.srcPos
          report.error(s"$longKindStr may only call js.native.", pos)
      }

      // Check that the resul type was explicitly specified
      // (This is stronger than Scala 2, which only warns, and only if it was inferred as Nothing.)
      if (tree.tpt.span.isSynthetic)
        report.error(i"The type of ${tree.name} must be explicitly specified because it is JS native.", tree)
    }

    private def checkJSNativeSpecificAnnotsOnNonJSNative(memberDef: MemberDef)(using Context): Unit = {
      for (annot <- memberDef.symbol.annotations) {
        annot.symbol match {
          case annotSym if annotSym == jsdefn.JSGlobalAnnot =>
            report.error("@JSGlobal can only be used on native JS definitions (with @js.native).", annot.tree)
          case annotSym if annotSym == jsdefn.JSImportAnnot =>
            report.error("@JSImport can only be used on native JS definitions (with @js.native).", annot.tree)
          case annotSym if annotSym == jsdefn.JSGlobalScopeAnnot =>
            report.error("@JSGlobalScope can only be used on native JS objects (with @js.native).", annot.tree)
          case _ =>
            // ok
        }
      }
    }

    private def checkJSNameAnnots(sym: Symbol)(using Context): Unit = {
      val allJSNameAnnots = sym.annotations.filter(_.symbol == jsdefn.JSNameAnnot).reverse

      for (annot <- allJSNameAnnots.headOption) {
        // Check everything about the first @JSName annotation
        if (sym.isLocalToBlock || (enclosingOwner isnt OwnerKind.JSType)) {
          report.error("@JSName can only be used on members of JS types.", annot.tree)
        } else if (sym.is(Trait)) {
          report.error("@JSName cannot be used on traits.", annot.tree)
        } else if (isPrivateMaybeWithin(sym)) {
          report.error("@JSName cannot be used on private members.", annot.tree)
        } else {
          if (shouldCheckLiterals)
            checkJSNameArgument(sym, annot)
        }

        // Check that there is at most one @JSName annotation.
        for (duplicate <- allJSNameAnnots.tail) {
          report.error("Duplicate @JSName annotation.", duplicate.tree)
        }
      }
    }

    /** Checks that the argument to `@JSName` annotations on `memberSym` is legal.
     *
     *  Reports an error on each annotation where this is not the case.
     *  One one `@JSName` annotation is allowed, but that is handled somewhere else.
     */
    private def checkJSNameArgument(memberSym: Symbol, annot: Annotation)(using Context): Unit = {
      val argTree = annot.arguments.head
      if (argTree.tpe <:< defn.StringType) {
        // We have a String. It must be a literal.
        if (!annot.argumentConstantString(0).isDefined)
          report.error("A String argument to JSName must be a literal string", argTree)
      } else {
        // We have a js.Symbol. It must be a stable reference.
        val sym = argTree.symbol
        if (!sym.isStatic || !sym.isStableMember) {
          report.error("A js.Symbol argument to JSName must be a static, stable identifier", argTree)
        } else if ((enclosingOwner is OwnerKind.JSNonNative) && sym.owner == memberSym.owner) {
          report.warning(
              "This symbol is defined in the same object as the annotation's target. " +
              "This will cause a stackoverflow at runtime",
              argTree)
        }
      }
    }

    /** Mark the symbol as exposed if it is a non-private term member of a
     *  non-native JS class.
     *
     *  @param sym
     *    The symbol, which must be the module symbol for a module, not its
     *    module class symbol.
     */
    private def markExposedIfRequired(sym: Symbol)(using Context): Unit = {
      def shouldBeExposed: Boolean = {
        // it is a member of a non-native JS class
        (enclosingOwner is OwnerKind.JSNonNative) && !sym.isLocalToBlock &&
        // it is a term member, and it is not synthetic
        sym.isOneOf(Module | Method, butNot = Synthetic) &&
        // it is not private
        !isPrivateMaybeWithin(sym) &&
        // it is not a constructor
        !sym.isConstructor //&& !sym.isValueParameter && !sym.isParamWithDefault
      }

      if (shouldPrepareExports && shouldBeExposed)
        sym.addAnnotation(jsdefn.ExposedJSMemberAnnot)
    }
  }
}

object PrepJSInterop {
  val name: String = "prepjsinterop"

  private final class OwnerKind private (private val baseKinds: Int)
      extends AnyVal {

    import OwnerKind._

    @inline def isBaseKind: Boolean =
      Integer.lowestOneBit(baseKinds) == baseKinds && baseKinds != 0 // exactly 1 bit on

    @inline def |(that: OwnerKind): OwnerKind =
      new OwnerKind(this.baseKinds | that.baseKinds)

    @inline def is(that: OwnerKind): Boolean =
      (this.baseKinds & that.baseKinds) != 0

    @inline def isnt(that: OwnerKind): Boolean =
      !this.is(that)
  }

  private object OwnerKind {
    /** No owner, i.e., we are at the top-level. */
    val None = new OwnerKind(0x00)

    // Base kinds - those form a partition of all possible enclosing owners

    /** A Scala class/trait. */
    val ScalaClass = new OwnerKind(0x01)
    /** A Scala object. */
    val ScalaMod = new OwnerKind(0x02)
    /** A native JS class/trait, which extends js.Any. */
    val JSNativeClass = new OwnerKind(0x04)
    /** A native JS object, which extends js.Any. */
    val JSNativeMod = new OwnerKind(0x08)
    /** A non-native JS class/trait. */
    val JSClass = new OwnerKind(0x10)
    /** A non-native JS object. */
    val JSMod = new OwnerKind(0x20)

    // Compound kinds

    /** A Scala class, trait or object, i.e., anything not extending js.Any. */
    val ScalaType = ScalaClass | ScalaMod

    /** A native JS class/trait/object. */
    val JSNative = JSNativeClass | JSNativeMod
    /** A non-native JS class/trait/object. */
    val JSNonNative = JSClass | JSMod
    /** A JS type, i.e., something extending js.Any. */
    val JSType = JSNative | JSNonNative

    /** Any kind of class/trait, i.e., a Scala or JS class/trait. */
    val AnyClass = ScalaClass | JSNativeClass | JSClass
  }

  def isJSAny(sym: Symbol)(using Context): Boolean =
    sym.isSubClass(jsdefn.JSAnyClass)

  /** Checks that a setter has the right signature.
   *
   *  Reports error messages otherwise.
   */
  def checkSetterSignature(sym: Symbol, pos: SrcPos, exported: Boolean)(using Context): Unit = {
    val typeStr = if (exported) "Exported" else "JS"

    val tpe = sym.info

    // The result type must be Unit
    if (!tpe.resultType.isRef(defn.UnitClass))
      report.error(s"$typeStr setters must return Unit", pos)

    // There must be exactly one non-varargs, non-default parameter
    tpe.paramInfoss match {
      case List(List(argInfo)) =>
        // Arg list is OK. Do additional checks.
        if (tpe.isVarArgsMethod)
          report.error(s"$typeStr setters may not have repeated params", pos)
        if (sym.hasDefaultParams)
          report.error(s"$typeStr setters may not have default params", pos)

      case _ =>
        report.error(s"$typeStr setters must have exactly one argument", pos)
    }
  }

  /** Tests whether the symbol has `private` in any form, either `private`,
   *  `private[this]` or `private[Enclosing]`.
   */
  def isPrivateMaybeWithin(sym: Symbol)(using Context): Boolean =
    sym.is(Private) || (sym.privateWithin.exists && !sym.is(Protected))

  /** Checks that the optional argument to an `@JSGlobal` annotation is a
   *  literal.
   *
   *  Reports an error on the annotation if it is not the case.
   */
  private def checkJSGlobalLiteral(annot: Annotation)(using Context): Unit = {
    if (annot.arguments.nonEmpty) {
      assert(annot.arguments.size == 1,
          s"@JSGlobal annotation $annot has more than 1 argument")

      val argIsValid = annot.argumentConstantString(0).isDefined
      if (!argIsValid)
        report.error("The argument to @JSGlobal must be a literal string.", annot.arguments.head)
    }
  }

  /** Checks that arguments to an `@JSImport` annotation are literals.
   *
   *  The second argument can also be the singleton `JSImport.Namespace`
   *  object.
   *
   *  Reports an error on the annotation if it is not the case.
   */
  private def checkJSImportLiteral(annot: Annotation)(using Context): Unit = {
    val args = annot.arguments
    assert(args.size == 2 || args.size == 3,
        i"@JSImport annotation $annot does not have exactly 2 or 3 arguments")

    val firstArgIsValid = annot.argumentConstantString(0).isDefined
    if (!firstArgIsValid)
      report.error("The first argument to @JSImport must be a literal string.", args.head)

    val secondArgIsValid = annot.argumentConstantString(1).isDefined || args(1).symbol == jsdefn.JSImportNamespaceModule
    if (!secondArgIsValid)
      report.error("The second argument to @JSImport must be literal string or the JSImport.Namespace object.", args(1))

    val thirdArgIsValid = args.size < 3 || annot.argumentConstantString(2).isDefined
    if (!thirdArgIsValid)
      report.error("The third argument to @JSImport, when present, must be a literal string.", args(2))
  }

  private def checkAndGetJSNativeLoadingSpecAnnotOf(pos: SrcPos, sym: Symbol)(
      using Context): Option[Annotation] = {

    // Must not have @JSName

    for (annot <- sym.getAnnotation(jsdefn.JSNameAnnot))
      report.error("@JSName can only be used on members of JS types.", annot.tree)

    // Must have exactly one JS native load spec annotation

    val annots = sym.annotations.filter(annot => isJSNativeLoadingSpecAnnot(annot.symbol))

    val badAnnotCountMsg =
      if (sym.is(Module)) "Native JS objects must have exactly one annotation among @JSGlobal, @JSImport and @JSGlobalScope."
      else "Native JS classes, vals and defs must have exactly one annotation among @JSGlobal and @JSImport."

    annots match {
      case Nil =>
        report.error(badAnnotCountMsg, pos)
        None
      case result :: Nil =>
        Some(result)
      case _ =>
        // Annotations are stored in reverse order, which we re-reverse now
        val result :: duplicates = annots.reverse
        for (annot <- duplicates)
          report.error(badAnnotCountMsg, annot.tree)
        Some(result)
    }
  }

  /* Note that we consider @JSGlobalScope as a JS native loading spec because
   * it's convenient for the purposes of PrepJSInterop. Actually @JSGlobalScope
   * objects do not receive a JS loading spec in their IR.
   */
  private def isJSNativeLoadingSpecAnnot(sym: Symbol)(using Context): Boolean = {
    sym == jsdefn.JSGlobalAnnot
      || sym == jsdefn.JSImportAnnot
      || sym == jsdefn.JSGlobalScopeAnnot
  }

  /*
  private def wasPublicBeforeTyper(sym: Symbol)(using Context): Boolean =
    sym.hasAnnotation(WasPublicBeforeTyperClass)

  private def fixPublicBeforeTyper(ddef: DefDef)(using Context): DefDef = {
    // This method assumes that isJSAny(ddef.symbol.owner) is true
    val sym = ddef.symbol
    val needsFix = {
      sym.isPrivate &&
      (wasPublicBeforeTyper(sym) ||
          (sym.is(Accessor) && wasPublicBeforeTyper(sym.accessed)))
    }
    if (needsFix) {
      sym.resetFlag(Flag.PRIVATE)
      treeCopy.DefDef(ddef, ddef.mods &~ Flag.PRIVATE, ddef.name, ddef.tparams,
          ddef.vparamss, ddef.tpt, ddef.rhs)
    } else {
      ddef
    }
  }
  */

  private def checkInternalAnnotations(sym: Symbol)(using Context): Unit = {
    /** Returns true iff it is a compiler annotations. This does not include
     *  annotations inserted before the typer (such as `@WasPublicBeforeTyper`).
     */
    def isCompilerAnnotation(annotation: Annotation): Boolean = {
      annotation.symbol == jsdefn.ExposedJSMemberAnnot
        || annotation.symbol == jsdefn.JSTypeAnnot
        || annotation.symbol == jsdefn.JSOptionalAnnot
    }

    for (annotation <- sym.annotations) {
      if (isCompilerAnnotation(annotation)) {
        report.error(
            i"@${annotation.symbol.fullName} is for compiler internal use only. Do not use it yourself.",
            annotation.tree)
      }
    }
  }
}
