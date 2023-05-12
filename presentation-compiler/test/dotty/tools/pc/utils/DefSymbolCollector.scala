package dotty.tools.pc.utils

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.{InteractiveCompiler, InteractiveDriver}
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.semanticdb.SemanticSymbolBuilder
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.io.VirtualFile
import org.eclipse.lsp4j.*

import java.io.File
import java.nio.file.Paths
import scala.collection.mutable
import scala.meta.internal.metals.{CompilerVirtualFileParams, Fuzzy}
import scala.meta.internal.mtags.MtagsEnrichments.*
import scala.meta.pc.{OffsetParams, SymbolSearchVisitor, VirtualFileParams}
import scala.meta.internal.pc.PcCollector

final class DefSymbolCollector(
    driver: InteractiveDriver,
    params: VirtualFileParams
) extends PcCollector[Option[Symbol]](driver, params):

  def collect(parent: Option[Tree])(
      tree: Tree,
      toAdjust: SourcePosition,
      sym: Option[Symbol]
  ): Option[Symbol] =
    tree match
      case tree: NamedDefTree => Some(tree.symbol)
      case _ => None

  def namedDefSymbols: List[(Symbol, String)] =
    result().flatten.map(symbol => symbol -> symbol.name.toString)
