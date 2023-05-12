package dotty.tools.pc.utils

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.ast.untpd.UntypedTreeAccumulator
import dotty.tools.dotc.core.Comments.CommentsContext
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.parsing.JavaParsers.JavaParser
import dotty.tools.dotc.semanticdb.*
import dotty.tools.dotc.semanticdb.Descriptor.Method
import dotty.tools.dotc.util.SourceFile

import java.nio.file.{FileSystem, FileSystems, Files}
import java.util.Collections
import scala.jdk.CollectionConverters.*
import scala.meta.pc.SymbolDocumentation
import scala.util.Try

class JdkIndex():
  val jdkSources = JdkSources()
  private val maybeJdkZipFs: Option[FileSystem] =
    jdkSources.toOption.map(FileSystems.newFileSystem(_, Collections.emptyMap))

  def search(classfile: String, symbol: Symbol, query: String)(using
      Context
  ): Option[SymbolDocumentation] =
    for
      jdkZipFs <- maybeJdkZipFs
      fsroot = jdkZipFs.getPath("/")
      adjustedClassfile = classfile.stripPrefix("/modules/")
      javafile <- adjustedClassfile.split("$").headOption.map(_.stripSuffix("class") ++ "java")
      content <- Try { Files.readString(fsroot.resolve(javafile)) }.toOption
      virtualFile = SourceFile.virtual(javafile, content)
      untpdTree <- Try { JavaParser(virtualFile).parse() }.toOption
      symbol <- extractSymbolInformation(untpdTree, symbol, query)
    yield symbol

  private def getDoc(symbol: Symbol)(using ctx: Context) =
    for
      docCtx <- ctx.docCtx
      comment <- docCtx.docstring(symbol)
    yield comment

  def extractSymbolInformation(tree: untpd.Tree, symbol: Symbol, semanticdbSymbol: String)(using
      Context
  ): Option[SymbolDocumentation] =
    val (descriptor, _) = DescriptorParser(semanticdbSymbol)

    val discriminator = (descriptor match
      case Method(value, "()") => Some(0)
      case Method(value, disambiguator) =>
        disambiguator.stripPrefix("(+").stripSuffix(")").toIntOption
      case _ => None
    ).getOrElse(0)

    val findDefDefs = new UntypedTreeAccumulator[List[untpd.DefDef]]:
      def apply(x: List[untpd.DefDef], t: untpd.Tree)(using Context) =
        t match
          case t: untpd.DefDef if t.name == symbol.name.toTermName && !t.mods.is(Flags.Private) =>
            foldOver(x :+ t, t)
          case t =>
            foldOver(x, t)

    val matchingTrees = findDefDefs(Nil, tree).sortBy(_.span.start).filterNot(_.span.isSynthetic)

    if matchingTrees.nonEmpty && discriminator < matchingTrees.length then
      val matchingTree = matchingTrees(discriminator)
      val typeParams = matchingTree.paramss.flatten.collect { case tree: untpd.TypeDef =>
        ScalaSymbolDocumentation("", tree.name.toString, "")
      }
      val params = matchingTree.paramss.flatten.collect { case tree: untpd.ValDef =>
        ScalaSymbolDocumentation("", tree.name.toString, "")
      }

      val doc = getDoc(symbol).map(_.raw)
      val matchingSymbolDocumentation = ScalaSymbolDocumentation(
        semanticdbSymbol,
        matchingTree.name.toString,
        doc.getOrElse(""),
        "",
        typeParams.asJava,
        params.asJava
      )
      Some(matchingSymbolDocumentation)
    else None
