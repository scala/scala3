package dotty.tools.pc.base

import java.nio.file.Paths

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.AutoImportsResult

import dotty.tools.pc.utils.TextEdits

trait BaseAutoImportsSuite extends BaseCodeActionSuite:

  val isExtensionMethods: Boolean = false

  def check(
      original: String,
      expected: String
  ): Unit =
    val imports = getAutoImports(original, "A.scala")
    val obtained = imports.map(_.packageName()).mkString("\n")
    assertNoDiff(expected, obtained)

  def checkEdit(
      original: String,
      expected: String,
      selection: Int = 0,
      filename: String = "A.scala"
  ): Unit =
    checkEditSelection(filename, original, expected, selection)

  def checkAmmoniteEdit(
      original: String,
      expected: String,
      selection: Int = 0
  ): Unit =
    checkEditSelection(
      "script.amm.sc.scala",
      original,
      expected,
      selection
    )

  def checkWorksheetEdit(
      original: String,
      expected: String,
      selection: Int = 0
  ): Unit =
    checkEditSelection(
      "example.worksheet.sc",
      original,
      expected,
      selection
    )

  def checkEditSelection(
      filename: String,
      original: String,
      expected: String,
      selection: Int
  ): Unit =
    val imports = getAutoImports(original, filename)
    if imports.size <= selection then fail("Obtained no expected imports")
    val edits = imports(selection).edits().asScala.toList
    val (code, _, _) = params(original)
    val obtained = TextEdits.applyEdits(code, edits)

    assertNoDiff(expected, obtained)

  def getAutoImports(
      original: String,
      filename: String
  ): List[AutoImportsResult] =
    val (code, symbol, offset) = params(original)
    val result = presentationCompiler
      .autoImports(
        symbol,
        CompilerOffsetParams(
          Paths.get(filename).toUri(),
          code,
          offset,
          cancelToken
        ),
        isExtensionMethods
      )
      .get()
    result.asScala.toList
