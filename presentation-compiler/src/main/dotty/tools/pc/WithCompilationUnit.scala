package dotty.tools.pc

import java.nio.file.Paths

import scala.language.unsafeNulls
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.OffsetParams
import scala.meta.pc.VirtualFileParams
import scala.meta as m

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.utils.InteractiveEnrichments.*

class WithCompilationUnit(
    val driver: InteractiveDriver,
    params: VirtualFileParams
):
  val uri: java.net.URI = params.uri()
  val filePath: java.nio.file.Path = Paths.get(uri)
  val sourceText: String = params.text
  val text: Array[Char] = sourceText.toCharArray()
  val source: SourceFile =
    SourceFile.virtual(filePath.toString, sourceText)
  driver.run(uri, source)
  given ctx: Context = driver.currentCtx

  private val run = driver.currentCtx.run
  val unit = run.units.head
  val compilatonUnitContext = ctx.fresh.setCompilationUnit(unit)
  val offset = params match
    case op: OffsetParams => op.offset()
    case _ => 0
  val offsetParams =
    params match
      case op: OffsetParams => op
      case _ =>
        CompilerOffsetParams(params.uri(), params.text(), 0, params.token())
  val pos = driver.sourcePosition(offsetParams)

  // First identify the symbol we are at, comments identify @@ as current cursor position
  def symbolAlternatives(sym: Symbol)(using Context) =
    def member(parent: Symbol) = parent.info.member(sym.name).symbol
    def primaryConstructorTypeParam(owner: Symbol) =
      for
        typeParams <- owner.primaryConstructor.paramSymss.headOption
        param      <- typeParams.find(_.name == sym.name)
        if (param.isType)
      yield param
    def additionalForEnumTypeParam(enumClass: Symbol) =
      if enumClass.is(Flags.Enum) then
        val enumOwner =
          if enumClass.is(Flags.Case)
          then
            // we check that the type parameter is the one from enum class
            // and not an enum case type parameter with the same name
            Option.when(member(enumClass).is(Flags.Synthetic))(
              enumClass.maybeOwner.companionClass
            )
          else Some(enumClass)
        enumOwner.toSet.flatMap { enumOwner =>
          val symsInEnumCases = enumOwner.children.toSet.flatMap(enumCase =>
            if member(enumCase).is(Flags.Synthetic)
            then primaryConstructorTypeParam(enumCase)
            else None
          )
          val symsInEnumOwner =
            primaryConstructorTypeParam(enumOwner).toSet + member(enumOwner)
          symsInEnumCases ++ symsInEnumOwner
        }
      else Set.empty
    val all =
      if sym.is(Flags.Exported) then
        Set(sym, sym.sourceSymbol)
      else if sym.is(Flags.ModuleClass) then
        Set(sym, sym.companionModule, sym.companionModule.companion)
      else if sym.isClass then
        Set(sym, sym.companionModule, sym.companion.moduleClass)
      else if sym.is(Flags.Module) then
        Set(sym, sym.companionClass, sym.moduleClass)
      else if sym.isTerm && (sym.owner.isClass || sym.owner.isConstructor)
      then
        val info =
          if sym.owner.isClass then sym.owner.info else sym.owner.owner.info
        Set(
          sym,
          info.member(sym.asTerm.name.setterName).symbol,
          info.member(sym.asTerm.name.getterName).symbol
        ) ++ sym.allOverriddenSymbols.toSet
      // type used in primary constructor will not match the one used in the class
      else if sym.isTypeParam && sym.owner.isPrimaryConstructor then
        Set(sym, member(sym.maybeOwner.maybeOwner))
          ++ additionalForEnumTypeParam(sym.maybeOwner.maybeOwner)
      else if sym.isTypeParam then
        primaryConstructorTypeParam(sym.maybeOwner).toSet
          ++ additionalForEnumTypeParam(sym.maybeOwner) + sym
      else Set(sym)
    all.filter(s => s != NoSymbol && !s.isError)

end WithCompilationUnit
