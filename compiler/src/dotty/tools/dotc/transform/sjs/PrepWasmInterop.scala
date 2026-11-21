package dotty.tools.dotc.transform.sjs

import java.nio.CharBuffer
import java.nio.charset.CharacterCodingException
import java.nio.charset.StandardCharsets.UTF_8

import scala.collection.mutable

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import Annotations.*
import Contexts.*
import Decorators.*
import Denotations.*
import Flags.*
import NameKinds.DefaultGetterName
import StdNames.*
import Symbols.*
import Types.*

import dotty.tools.dotc.report

import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SrcPos

import dotty.tools.backend.sjs.JSDefinitions.jsdefn
import JSExportUtils.*
import JSSymUtils.*

import dotty.tools.sjs.ir.WellKnownNames.DefaultModuleID
import dotty.tools.sjs.ir.Trees.TopLevelExportDef.isValidTopLevelExportName

object PrepWasmInterop {
  import tpd.*

  def checkWasmInteropAnnotations(tree: MemberDef)(using Context): Unit = {
    val sym = moduleToModuleClass(tree.symbol)
    val importAnnot = sym.getAnnotation(jsdefn.WasmImportAnnot)
    val exportAnnot = sym.getAnnotation(jsdefn.WasmExportAnnot)

    for (annot <- importAnnot)
      checkWasmImportAnnotation(tree, sym, annot)

    for (annot <- exportAnnot)
      checkWasmExportAnnotation(tree, sym, annot)
  }

  private def checkWasmImportAnnotation(tree: MemberDef, sym: Symbol, annot: Annotation)(using Context): Unit = {
    checkWasmImportStringArg(annot, 0)
    checkWasmImportStringArg(annot, 1)

    tree match {
      case tree: DefDef =>
        if (sym.isConstructor || sym.is(Accessor)) {
          report.error(
              "@WasmImport can only be used on methods", tree)
        } else if (sym.isLocalToBlock) {
          report.error(
              "@WasmImport is not allowed on local definitions", tree)
        } else if (!isWasmInteropAllowedOwner(sym)) {
          report.error(
              "@WasmImport can only be used on methods in static Scala objects", tree)
        } else if (!sym.isPublic) {
          report.error(
              "@WasmImport can only be used on public methods", tree)
        } else if (sym.paramSymss.exists(_.exists(_.isType))) {
          report.error(
              "@WasmImport methods may not have type parameters", tree)
        } else if (tree.termParamss.isEmpty) {
          report.error(
              "@WasmImport methods must have a parameter list", tree)
        } else if (tree.termParamss.sizeIs > 1) {
          report.error(
              "@WasmImport methods may not have multiple parameter lists", tree)
        } else if (sym.hasAnnotation(jsdefn.JSNativeAnnot)) {
          report.error(
              "@WasmImport cannot be used together with @js.native", annot.tree)
        } else if (sym.is(Deferred)) {
          report.error(
              "@WasmImport can only be used on concrete methods", tree)
        } else {
          checkRHSCallsWasmNative(tree)
          checkWasmInteropMethodType(sym, "@WasmImport")
        }
      case _ =>
        report.error(
            "@WasmImport can only be used on methods", tree)
    }
  }

  private def checkWasmExportAnnotation(tree: MemberDef, sym: Symbol, annot: Annotation)(using Context): Unit = {
    val exportName = annot.argumentConstantString(0)

    checkWasmExportStringArg(annot)

    tree match {
      case tree: DefDef =>
        if (sym.isConstructor || sym.is(Accessor)) {
          report.error(
              "@WasmExport can only be used on methods", tree)
        } else if (sym.isLocalToBlock) {
          report.error(
              "@WasmExport is not allowed on local definitions", tree)
        } else if (!isWasmInteropAllowedOwner(sym)) {
          report.error(
              "@WasmExport can only be used on methods in static Scala objects", tree)
        } else if (!sym.isPublic) {
          report.error(
              "@WasmExport can only be used on public methods", tree)
        } else if (sym.paramSymss.exists(_.exists(_.isType))) {
          report.error(
              "@WasmExport methods may not have type parameters", tree)
        } else if (tree.termParamss.isEmpty) {
          report.error(
              "@WasmExport methods must have a parameter list", tree)
        } else if (tree.termParamss.sizeIs > 1) {
          report.error(
              "@WasmExport methods may not have multiple parameter lists", tree)
        } else if (sym.hasAnnotation(jsdefn.JSNativeAnnot)) {
          report.error(
              "@WasmExport cannot be used together with @js.native", annot.tree)
        } else if (sym.is(Deferred)) {
          report.error(
              "@WasmExport can only be used on concrete methods", tree)
        } else {
          checkWasmInteropMethodType(sym, "@WasmExport")

          /* TODO The Scala 2 plugin checks for duplicate exports inside the same class.
           * It uses a global map to do so, which is awkward in the Scala 3 infrastructure.
           * Currently we don't do it; it's OK, it will be a linking error anyway.
          for (name <- exportName) {
            val namesForOwner = wasmExportNames.getOrElseUpdate(sym.owner, mutable.Map.empty)
            namesForOwner.get(name) match {
              case Some(pos) =>
                report.error(
                    i"Duplicate @WasmExport name '$name' already used at $pos", annot.tree)
              case None =>
                namesForOwner.put(name, annot.tree.span)
            }
          }
          */
        }

      case _ =>
        report.error(
            "@WasmExport can only be used on methods", tree)
    }
  }

  private def isWasmInteropAllowedOwner(sym: Symbol)(using Context): Boolean =
    sym.owner.is(ModuleClass) && sym.owner.isStatic && !sym.owner.isSubClass(jsdefn.JSAnyClass)

  private def checkWasmImportStringArg(annot: Annotation, idx: Int)(using Context): Unit = {
    annot.argumentConstantString(idx) match {
      case Some(value) =>
        if (!isValidUTF16String(value)) {
          report.error(
              "The arguments to @WasmImport must be valid UTF-16 strings", annot.arguments(idx))
        }
      case None =>
        report.error(
            "The arguments to @WasmImport must be literal strings", annot.arguments(idx))
    }
  }

  private def checkWasmExportStringArg(annot: Annotation)(using Context): Unit = {
    annot.argumentConstantString(0) match {
      case Some(value) =>
        if (!isValidUTF16String(value)) {
          report.error(
              "The argument to @WasmExport must be a valid UTF-16 string", annot.arguments.head)
        }
      case None =>
        report.error(
            "The argument to @WasmExport must be a literal string", annot.arguments.head)
    }
  }

  private def isValidUTF16String(str: String)(using Context): Boolean = {
    try {
      UTF_8.newEncoder().encode(CharBuffer.wrap(str))
      true
    } catch {
      case _: CharacterCodingException => false
    }
  }

  private def checkWasmInteropMethodType(sym: Symbol, subject: String)(using Context): Unit = {
    for (param <- sym.paramSymss.flatten) {
      if (param.info.hasAnnotation(defn.RepeatedAnnot)) {
        report.error(
            s"$subject methods may not have repeated parameters", param)
      } else if (param.is(HasDefault)) {
        report.error(
            s"$subject methods may not have default parameters", param)
      } else if (!isWasmInteropType(param.info, isParam = true)) {
        report.error(
            "Wasm imports and exports only support Int, Long, Float, Double, " +
            "arrays of Byte, Short, Int, Long, Float and Double, " +
            "and Unit as result type",
            param)
      }
    }
    if (!isWasmInteropType(sym.info.resultType, isParam = false)) {
      report.error(
          "Wasm imports and exports only support Int, Long, Float, Double, " +
          "arrays of Byte, Short, Int, Long, Float and Double, " +
          "and Unit as result type",
          sym)
    }
  }

  private def isWasmInteropType(tpe: Type, isParam: Boolean)(using Context): Boolean = {
    val tpe1 = tpe.dealias
    val tsym = tpe1.typeSymbol
    (isWasmInteropPrimitiveType(tpe1) ||
      (!isParam && tsym == defn.UnitClass) ||
      isSupportedWasmInteropArrayType(tpe1))
  }

  private def isWasmInteropPrimitiveType(tpe: Type)(using Context): Boolean = tpe match {
    case ConstantType(_) =>
      false // reject literal type: ConstantType(1).typeSymbol => IntType
    case _ =>
      val sym = tpe.typeSymbol
      sym == defn.IntClass || sym == defn.LongClass || sym == defn.FloatClass || sym == defn.DoubleClass
  }

  /** Byte, Short, Int, Long, Float and Double. */
  private def isWasmInteropArrayTypeClass(sym: Symbol)(using Context): Boolean =
    defn.ScalaNumericValueClasses().contains(sym) && sym != defn.CharClass

  private def isSupportedWasmInteropArrayType(tpe: Type)(using Context): Boolean = {
    tpe match {
      case AppliedType(tycon, List(arg)) if tycon.typeSymbol == defn.ArrayClass =>
        isWasmInteropArrayTypeClass(arg.dealias.typeSymbol)
      case _ =>
        false
    }
  }

  private def checkRHSCallsWasmNative(tree: ValOrDefDef)(using Context): Unit = {
    tree.rhs match {
      case sel: Select if sel.symbol == jsdefn.WasmPackage_native =>
        // ok
      case _ =>
        val srcPos = if (tree.rhs != EmptyTree) tree.rhs else tree
        report.error("@WasmImport methods may only call wasm.native.", srcPos)
    }
  }

  private def moduleToModuleClass(sym: Symbol)(using Context): Symbol =
    if (sym.is(Module)) sym.moduleClass
    else sym
}
