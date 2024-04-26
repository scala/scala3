package dotty.tools.dotc
package transform
package sjs

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import Contexts.*
import Decorators.*
import Denotations.*
import Flags.*
import NameKinds.DefaultGetterName
import StdNames.*
import Symbols.*

import Types.*

import util.Spans.Span
import util.SrcPos

import dotty.tools.backend.sjs.JSDefinitions.jsdefn
import JSExportUtils.*
import JSSymUtils.*

import org.scalajs.ir.Names.DefaultModuleID
import org.scalajs.ir.Trees.TopLevelExportDef.isValidTopLevelExportName

object PrepJSExports {
  import tpd.*
  import PrepJSInterop.{checkSetterSignature, isJSAny, isPrivateMaybeWithin}

  private sealed abstract class ExportDestination

  private object ExportDestination {
    /** Export in the "normal" way: as an instance member, or at the top-level
     *  for naturally top-level things (classes and modules).
     */
    case object Normal extends ExportDestination

    /** Export at the top-level. */
    final case class TopLevel(moduleID: String) extends ExportDestination

    /** Export as a static member of the companion class. */
    case object Static extends ExportDestination
  }

  private final case class ExportInfo(jsName: String, destination: ExportDestination)(val pos: SrcPos)

  /** Generate exports for the given Symbol.
   *
   *  - Registers top-level and static exports.
   *  - Returns (non-static) exporters for this symbol.
   */
  def genExport(sym: Symbol)(using Context): List[Tree] = {
    // Scala classes are never exported: Their constructors are.
    val isScalaClass = sym.isClass && !sym.isOneOf(Trait | Module) && !isJSAny(sym)

    // Filter constructors of module classes: The module classes themselves will be exported.
    val isModuleClassCtor = sym.isConstructor && sym.owner.is(ModuleClass)

    val exports =
      if (isScalaClass || isModuleClassCtor) Nil
      else exportsOf(sym)

    assert(exports.isEmpty || !sym.is(Bridge),
        s"found exports for bridge symbol $sym. exports: $exports")

    // For normal exports, generate exporter methods.
    val normalExports = exports.filter(_.destination == ExportDestination.Normal)
    normalExports.flatMap(exp => genExportDefs(sym, exp.jsName, exp.pos.span))
  }

  /** Computes the ExportInfos for sym from its annotations. */
  private def exportsOf(sym: Symbol)(using Context): List[ExportInfo] = {
    val trgSym = {
      def isOwnerScalaClass = !sym.owner.is(ModuleClass) && !isJSAny(sym.owner)

      // For primary Scala class constructors, look on the class itself
      if (sym.isPrimaryConstructor && isOwnerScalaClass) sym.owner
      else sym
    }

    val symOwner =
      if (sym.isConstructor) sym.owner.owner
      else sym.owner

    val JSExportAnnot = jsdefn.JSExportAnnot
    val JSExportTopLevelAnnot = jsdefn.JSExportTopLevelAnnot
    val JSExportStaticAnnot = jsdefn.JSExportStaticAnnot
    val JSExportAllAnnot = jsdefn.JSExportAllAnnot

    // Annotations that are directly on the member
    val directMemberAnnots = Set[Symbol](JSExportAnnot, JSExportTopLevelAnnot, JSExportStaticAnnot)
    val directAnnots = trgSym.annotations.filter(annot => directMemberAnnots.contains(annot.symbol))

    /* Annotations for this member on the whole unit
     *
     * Note that for top-level classes / modules this is always empty, because
     * packages cannot have annotations.
     */
    val unitAnnots = {
      val useExportAll = {
        sym.isPublic &&
        !sym.is(Synthetic) &&
        !sym.isConstructor &&
        !sym.is(Trait) &&
        (!sym.isClass || sym.is(ModuleClass))
      }

      if (useExportAll)
        symOwner.annotations.filter(_.symbol == JSExportAllAnnot)
      else
        Nil
    }

    val allAnnots = {
      val allAnnots0 = directAnnots ++ unitAnnots

      if (allAnnots0.nonEmpty) {
        val errorPos: SrcPos =
          if (allAnnots0.head.symbol == JSExportAllAnnot) sym
          else allAnnots0.head.tree
        if (checkExportTarget(sym, errorPos)) allAnnots0
        else Nil // prevent code generation from running to avoid crashes.
      } else {
        Nil
      }
    }

    val allExportInfos = for {
      annot <- allAnnots
    } yield {
      val isExportAll = annot.symbol == JSExportAllAnnot
      val isTopLevelExport = annot.symbol == JSExportTopLevelAnnot
      val isStaticExport = annot.symbol == JSExportStaticAnnot
      val hasExplicitName = annot.arguments.nonEmpty

      val exportPos: SrcPos = if (isExportAll) sym else annot.tree

      assert(!isTopLevelExport || hasExplicitName,
          em"Found a top-level export without an explicit name at ${exportPos.sourcePos}")

      val name = {
        if (hasExplicitName) {
          annot.argumentConstantString(0).getOrElse {
            report.error(
                em"The argument to ${annot.symbol.name} must be a literal string",
                annot.arguments(0))
            "dummy"
          }
        } else {
          val name = (if (sym.isConstructor) sym.owner else sym).defaultJSName
          if (name.endsWith(str.SETTER_SUFFIX) && !sym.isJSSetter) {
            report.error(
                "You must set an explicit name when exporting a non-setter with a name ending in _=",
                exportPos)
          }
          name
        }
      }

      val destination = {
        if (isTopLevelExport) {
          val moduleID = if (annot.arguments.size == 1) {
            DefaultModuleID
          } else {
            annot.argumentConstantString(1).getOrElse {
              report.error("moduleID must be a literal string", annot.arguments(1))
              DefaultModuleID
            }
          }

          ExportDestination.TopLevel(moduleID)
        } else if (isStaticExport) {
          ExportDestination.Static
        } else {
          ExportDestination.Normal
        }
      }

      // Enforce no __ in name
      if (!isTopLevelExport && name.contains("__"))
        report.error("An exported name may not contain a double underscore (`__`)", exportPos)

      // Destination-specific restrictions
      destination match {
        case ExportDestination.Normal =>
          // Disallow @JSExport on top-level definitions.
          if (symOwner.is(Package) || symOwner.isPackageObject) {
            report.error("@JSExport is forbidden on top-level definitions. Use @JSExportTopLevel instead.", exportPos)
          }

          // Make sure we do not override the default export of toString
          def isIllegalToString = {
            name == "toString" && sym.name != nme.toString_ &&
            sym.info.paramInfoss.forall(_.isEmpty) && !sym.isJSGetter
          }
          if (isIllegalToString) {
            report.error(
                "You may not export a zero-argument method named other than 'toString' under the name 'toString'",
                exportPos)
          }

          /* Illegal function application exports, i.e., method named 'apply'
           * without an explicit export name.
           */
          if (!hasExplicitName && sym.name == nme.apply) {
            def shouldBeTolerated = {
              isExportAll && directAnnots.exists { annot =>
                annot.symbol == JSExportAnnot &&
                annot.arguments.nonEmpty &&
                annot.argumentConstantString(0).contains("apply")
              }
            }

            // Don't allow apply without explicit name
            if (!shouldBeTolerated) {
              report.error(
                  "A member cannot be exported to function application. " +
                  "Add @JSExport(\"apply\") to export under the name apply.",
                  exportPos)
            }
          }

        case _: ExportDestination.TopLevel =>
          if (sym.is(Lazy))
            report.error("You may not export a lazy val to the top level", exportPos)
          else if (sym.is(Method, butNot = Accessor) && sym.isJSProperty)
            report.error("You may not export a getter or a setter to the top level", exportPos)

          // Disallow non-static definitions.
          if (!symOwner.isStatic || !symOwner.is(ModuleClass))
            report.error("Only static objects may export their members to the top level", exportPos)

          // The top-level name must be a valid JS identifier
          if (!isValidTopLevelExportName(name))
            report.error("The top-level export name must be a valid JavaScript identifier name", exportPos)

        case ExportDestination.Static =>
          def companionIsNonNativeJSClass: Boolean = {
            val companion = symOwner.companionClass
            companion != NoSymbol
              && !companion.is(Trait)
              && isJSAny(companion)
              && !companion.hasAnnotation(jsdefn.JSNativeAnnot)
          }

          if (!symOwner.isStatic || !symOwner.is(ModuleClass) || !companionIsNonNativeJSClass) {
            report.error(
                "Only a static object whose companion class is a non-native JS class may export its members as static.",
                exportPos)
          }

          if (sym.is(Lazy))
            report.error("You may not export a lazy val as static", exportPos)

          // Illegal function application export
          if (!hasExplicitName && sym.name == nme.apply) {
            report.error(
                "A member cannot be exported to function application as " +
                "static. Use @JSExportStatic(\"apply\") to export it under " +
                "the name 'apply'.",
                exportPos)
          }

          if (sym.isClass || sym.isConstructor) {
            report.error("Implementation restriction: cannot export a class or object as static", exportPos)
          }
      }

      ExportInfo(name, destination)(exportPos)
    }

    allExportInfos.filter(_.destination == ExportDestination.Normal)
      .groupBy(_.jsName)
      .filter { case (jsName, group) =>
        if (jsName == "apply" && group.size == 2)
          // @JSExportAll and single @JSExport("apply") should not be warned.
          !unitAnnots.exists(_.symbol == JSExportAllAnnot)
        else
          group.size > 1
      }
      .foreach(_ => report.warning("Found duplicate @JSExport", sym))

    /* Check that no field is exported *twice* as static, nor both as static
     * and as top-level (it is possible to export a field several times as
     * top-level, though).
     */
    if (!sym.is(Method)) {
      for (firstStatic <- allExportInfos.find(_.destination == ExportDestination.Static)) {
        for (duplicate <- allExportInfos) {
          duplicate.destination match {
            case ExportDestination.Normal =>
              // OK
            case ExportDestination.Static =>
              if (duplicate ne firstStatic) {
                report.error(
                    "Fields (val or var) cannot be exported as static more than once",
                    duplicate.pos)
              }
            case _: ExportDestination.TopLevel =>
              report.error(
                  "Fields (val or var) cannot be exported both as static and at the top-level",
                  duplicate.pos)
          }
        }
      }
    }

    allExportInfos.distinct
  }

  /** Checks whether the given target is suitable for export and exporting
   *  should be performed.
   *
   *  Reports any errors for unsuitable targets.
   *  @returns a boolean indicating whether exporting should be performed. Note:
   *      a result of true is not a guarantee that no error was emitted. But it is
   *      a guarantee that the target is not "too broken" to run the rest of
   *      the generation. This approximation is done to avoid having to complicate
   *      shared code verifying conditions.
   */
  private def checkExportTarget(sym: Symbol, errPos: SrcPos)(using Context): Boolean = {
    def err(msg: String): Boolean = {
      report.error(msg, errPos)
      false
    }

    def hasLegalExportVisibility(sym: Symbol): Boolean =
      sym.isPublic || sym.is(Protected, butNot = Local)

    def isMemberOfJSAny: Boolean =
      isJSAny(sym.owner) || (sym.isConstructor && isJSAny(sym.owner.owner))

    def hasIllegalRepeatedParam: Boolean = {
      val paramInfos = sym.info.paramInfoss.flatten
      paramInfos.nonEmpty && paramInfos.init.exists(_.isRepeatedParam)
    }

    def hasIllegalDefaultParam: Boolean = {
      sym.hasDefaultParams
        && sym.paramSymss.flatten.reverse.dropWhile(_.is(HasDefault)).exists(_.is(HasDefault))
    }

    def hasAnyNonPrivateCtor: Boolean =
      sym.info.member(nme.CONSTRUCTOR).hasAltWith(d => !isPrivateMaybeWithin(d.symbol))

    if (sym.is(Trait)) {
      err("You may not export a trait")
    } else if (sym.hasAnnotation(jsdefn.JSNativeAnnot)) {
      err("You may not export a native JS definition")
    } else if (!hasLegalExportVisibility(sym)) {
      err("You may only export public and protected definitions")
    } else if (sym.isConstructor && !hasLegalExportVisibility(sym.owner)) {
      err("You may only export constructors of public and protected classes")
    } else if (sym.is(Macro)) {
      err("You may not export a macro")
    } else if (isMemberOfJSAny) {
      err("You may not export a member of a subclass of js.Any")
    } else if (sym.isLocalToBlock) {
      err("You may not export a local definition")
    } else if (sym.isConstructor && sym.owner.isLocalToBlock) {
      err("You may not export constructors of local classes")
    } else if (hasIllegalRepeatedParam) {
      err("In an exported method or constructor, a *-parameter must come last " +
          "(through all parameter lists)")
    } else if (hasIllegalDefaultParam) {
      err("In an exported method or constructor, all parameters with " +
          "defaults must be at the end")
    } else if (sym.isConstructor && sym.owner.is(Abstract, butNot = Trait) && !isJSAny(sym)) {
      err("You may not export an abstract class")
    } else if (sym.isClass && !sym.is(ModuleClass) && isJSAny(sym) && !hasAnyNonPrivateCtor) {
      /* This test is only relevant for JS classes: We'll complain on the
       * individual exported constructors in case of a Scala class.
       */
      err("You may not export a class that has only private constructors")
    } else {
      if (sym.isJSSetter)
        checkSetterSignature(sym, errPos, exported = true)

      true // ok even if a setter has the wrong signature.
    }
  }

  /** Generates an exporter for a DefDef including default parameter methods. */
  private def genExportDefs(sym: Symbol, jsName: String, span: Span)(using Context): List[Tree] = {
    val siblingSym =
      if (sym.isConstructor) sym.owner
      else sym

    val clsSym = siblingSym.owner.asClass

    val isProperty = sym.is(ModuleClass) || isJSAny(sym) || sym.isJSProperty

    val copiedFlags0 = (siblingSym.flags & (Protected | Final)).toTermFlags
    val copiedFlags =
      if (siblingSym.is(HasDefaultParams)) copiedFlags0 | HasDefaultParams // term flag only
      else copiedFlags0

    // Create symbol for new method
    val scalaName = makeExportName(jsName, !sym.is(Method) || sym.isJSProperty)
    val flags = Method | Synthetic | copiedFlags
    val info =
      if (sym.isConstructor) sym.info
      else if (sym.is(Method)) finalResultTypeToAny(sym.info)
      else ExprType(defn.AnyType)
    val expSym = newSymbol(clsSym, scalaName, flags, info, sym.privateWithin, span).entered

    // Construct exporter DefDef tree
    val exporter = genProxyDefDef(clsSym, sym, expSym, span)

    // Construct exporters for default getters
    val defaultGetters = if (!sym.hasDefaultParams) {
      Nil
    } else {
      for {
        (param, i) <- sym.paramSymss.flatten.zipWithIndex
        if param.is(HasDefault)
      } yield {
        genExportDefaultGetter(clsSym, sym, expSym, i, span)
      }
    }

    exporter :: defaultGetters
  }

  private def genExportDefaultGetter(clsSym: ClassSymbol, trgMethod: Symbol,
      exporter: Symbol, paramPos: Int, span: Span)(using Context): Tree = {

    // Get default getter method we'll copy
    val trgGetterDenot = defaultGetterDenot(clsSym, trgMethod, paramPos)

    assert(trgGetterDenot.exists, em"Cannot find default getter for param $paramPos of $trgMethod")

    // Although the following must be true in a correct program, we cannot
    // assert, since a graceful failure message is only generated later
    if (!trgGetterDenot.isOverloaded) {
      val trgGetter = trgGetterDenot.symbol
      val expGetterName = DefaultGetterName(exporter.name.asTermName, paramPos)
      val expGetter = newSymbol(clsSym, expGetterName, trgGetter.flags, trgGetter.info,
          trgGetter.privateWithin, coord = span).entered
      genProxyDefDef(clsSym, trgGetter, expGetter, span)
    } else {
      EmptyTree
    }
  }

  private def defaultGetterDenot(targetSym: Symbol, sym: Symbol, paramIndex: Int)(using Context): Denotation =
    targetSym.info.member(DefaultGetterName(sym.name.asTermName, paramIndex))

  /** generate a DefDef tree (from [[proxySym]]) that calls [[trgSym]] */
  private def genProxyDefDef(clsSym: ClassSymbol, trgSym: Symbol,
      proxySym: TermSymbol, span: Span)(using Context): Tree = {

    DefDef(proxySym, { argss =>
      if (trgSym.isConstructor) {
        val tycon = trgSym.owner.typeRef
        New(tycon).select(TermRef(tycon, trgSym)).appliedToArgss(argss)
      } else if (trgSym.is(ModuleClass)) {
        assert(argss.isEmpty,
            s"got a module export with non-empty paramss. target: $trgSym, proxy: $proxySym at $span")
        ref(trgSym.sourceModule)
      } else if (trgSym.isClass) {
        assert(isJSAny(trgSym), s"got a class export for a non-JS class ($trgSym) at $span")
        val tpe = argss match {
          case Nil =>
            trgSym.typeRef
          case (targs @ (first :: _)) :: Nil if first.isType =>
            trgSym.typeRef.appliedTo(targs.map(_.tpe))
          case _ =>
            throw AssertionError(s"got a class export with unexpected paramss. target: $trgSym, proxy: $proxySym at $span")
        }
        ref(jsdefn.JSPackage_constructorOf).appliedToType(tpe)
      } else {
        This(clsSym).select(trgSym).appliedToArgss(argss)
      }
    }).withSpan(span)
  }

  /** Changes the final result type of a type `tpe` to Any. */
  private def finalResultTypeToAny(tpe: Type)(using Context): Type = tpe match {
    case tpe: MethodType =>
      MethodType(tpe.paramNames, tpe.paramInfos, finalResultTypeToAny(tpe.resultType))
    case _: ExprType =>
      ExprType(defn.AnyType)
    case tpe: PolyType =>
      PolyType(tpe.paramNames)(
          x => tpe.paramInfos.mapConserve(_.subst(tpe, x).bounds),
          x => finalResultTypeToAny(tpe.resultType.subst(tpe, x)))
    case _ =>
      defn.AnyType
  }
}
