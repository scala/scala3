package dotty.tools.pc.base

import java.net.URI

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.{CompilerOffsetParams, EmptyCancelToken}

import dotty.tools.pc.utils.{RangeReplace, TextEdits}

class BasePcRenameSuite extends BasePCSuite with RangeReplace:

  def check(
      methodBody: String,
      newName: String = "newName",
      filename: String = "A.scala",
      wrap: Boolean = true
  ): Unit =
    val original =
      if !wrap then methodBody
      else
        s"""|object Main {
            |def method() = {
            |$methodBody
            |}
            |}
            |""".stripMargin

    val edit = original.replaceAll("(<<|>>)", "")
    val expected =
      original.replaceAll("@@", "").replaceAll("\\<\\<\\S*\\>\\>", newName)
    val base = original.replaceAll("(<<|>>|@@)", "")
    val (code, offset) = params(edit)
    val renames = presentationCompiler
      .rename(
        CompilerOffsetParams(
          URI.create(s"file:/$filename"),
          code,
          offset,
          EmptyCancelToken
        ),
        newName
      )
      .get()
      .asScala
      .toList

    val obtained = TextEdits.applyEdits(base, renames)

    assertNoDiff(expected, obtained)

  def prepare(
      input: String,
      filename: String = "A.scala"
  ): Unit =
    val edit = input.replaceAll("(<<|>>)", "")
    val expected =
      input.replaceAll("@@", "")
    val base = input.replaceAll("(<<|>>|@@)", "")
    val (code, offset) = params(edit)
    val range = presentationCompiler
      .prepareRename(
        CompilerOffsetParams(
          URI.create(s"file:/$filename"),
          code,
          offset,
          EmptyCancelToken
        )
      )
      .get()

    val obtained =
      if !range.isPresent() then base else replaceInRange(base, range.get())

    assertNoDiff(expected, obtained)
