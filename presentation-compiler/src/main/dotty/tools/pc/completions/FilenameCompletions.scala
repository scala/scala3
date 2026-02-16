package dotty.tools.pc
package completions

import dotty.tools.dotc.ast.tpd.TypeDef
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.pc.utils.InteractiveEnrichments.*

object FilenameCompletions:

  def contribute(
      filename: String,
      td: TypeDef
  )(using ctx: Context): List[CompletionValue] =
    val owner = td.symbol.owner
    lazy val scope =
      owner.info.decls.filter(sym => sym.isType && sym.sourcePos.exists)
    if owner.is(Flags.Package) && !scope.exists(sym =>
        sym.name.decoded == filename && (sym.is(Flags.ModuleClass) == td.symbol
          .is(Flags.ModuleClass))
      )
    then
      List(
        CompletionValue.keyword(
          s"${td.symbol.showKind} ${filename}",
          filename
        )
      )
    else Nil
