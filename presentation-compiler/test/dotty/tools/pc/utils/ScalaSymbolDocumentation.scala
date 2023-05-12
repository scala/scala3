package dotty.tools.pc.utils

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd.{UntypedDeepFolder, UntypedTreeAccumulator, existsSubTree}
import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Comments.CommentsContext
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.JavaDefined
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.{InteractiveCompiler, InteractiveDriver}
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.parsing.Parsers.OutlineParser
import dotty.tools.dotc.semanticdb.*
import dotty.tools.dotc.semanticdb.Descriptor.Method
import dotty.tools.dotc.util.{CommentParsing, ParsedComment, SourceFile, SourcePosition}
import dotty.tools.io.{AbstractFile, VirtualFile}
import org.eclipse.lsp4j.*

import java.io.File
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.Collections
import java.util.zip.ZipFile
import scala.collection.mutable.HashMap
import scala.collection.{immutable, mutable}
import scala.io.Codec
import scala.jdk.CollectionConverters.*
import scala.meta.internal.metals.{CompilerVirtualFileParams, EmptySymbolDocumentation, Fuzzy}
import scala.meta.internal.mtags.MtagsEnrichments.*
import scala.meta.pc.{OffsetParams, SymbolDocumentation, SymbolSearchVisitor, VirtualFileParams}

case class ScalaSymbolDocumentation(
    symbol: String,
    displayName: String,
    docstring: String,
    defaultValue: String = "",
    typeParameters: java.util.List[SymbolDocumentation] = immutable.Nil.asJava,
    parameters: java.util.List[SymbolDocumentation] = immutable.Nil.asJava
) extends SymbolDocumentation
