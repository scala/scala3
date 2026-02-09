package dotty.tools.pc
package completions

import java.nio.file.Files
import java.nio.file.Path

import scala.jdk.CollectionConverters.*
import scala.meta.internal.pc.CompletionFuzzy

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.ast.untpd.ImportSelector
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j as l

object AmmoniteFileCompletions:

  private def translateImportToPath(tree: Tree): String =
    tree match
      case Select(qual, name) =>
        val pathPart = name.toString()
        translateImportToPath(qual) + "/" + {
          if pathPart == "^" then ".."
          else pathPart
        }
      case Ident(_) =>
        ""
      case _ => ""

  def contribute(
      select: Tree,
      selector: List[ImportSelector],
      posRange: l.Range,
      rawPath: String,
      workspace: Option[Path],
      rawFileName: String
  )(using Context): List[CompletionValue] =
    val fileName: Option[String] = Option(rawFileName)
      .flatMap(_.split("/").lastOption.map(_.stripSuffix(".amm.sc.scala")))

    val split: List[String] = Option(rawPath)
      .fold(Nil)(_.split("\\$file").toList.map(_.nn))

    val editRange = selector.headOption.map { sel =>
      if sel.sourcePos.span.isZeroExtent then posRange
      else sel.imported.sourcePos.toLsp
    }
    val query = selector.collectFirst { case sel: ImportSelector =>
      if sel.name.isEmpty || sel.name == nme.ERROR then ""
      else sel.name.toString.replace(Cursor.value, "")
    }

    def parent =
      val name = "^"

      CompletionValue.FileSystemMember(
        name,
        editRange,
        isDirectory = true
      )

    def matches(file: Path): Boolean =
      (Files.isDirectory(file) || file.toAbsolutePath().toString.isScalaScript) &&
        query.exists(q => CompletionFuzzy.matches(q.nn, file.getFileName().toString))

    (split, workspace) match
      case (_ :: script :: Nil, Some(workspace)) =>
        // drop / or \
        val current = workspace.resolve(script.drop(1))
        val importPath = translateImportToPath(select).drop(1)
        val currentPath = current.getParent().resolve(importPath).toAbsolutePath()
        val parentTextEdit =
          if query.exists(_.isEmpty()) &&
            Files.exists(currentPath.getParent()) && Files.isDirectory(
              currentPath
            )
          then List(parent)
          else Nil
        Files
          .list(currentPath).nn
          .iterator().nn
          .asScala
          .toList
          .filter(path => !fileName.contains(path.getFileName().toString.stripSuffix(".sc")))
          .collect {
            case file if matches(file) =>
              CompletionValue.FileSystemMember(
                file.getFileName().toString,
                editRange,
                isDirectory = Files.isDirectory(file)
              )
          } ++ parentTextEdit
      case _ =>
        Nil
  end contribute
end AmmoniteFileCompletions
