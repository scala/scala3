package dotty.tools.directives

import org.junit.Test
import org.junit.Assert.*

class CommentExtractorTests:

  private def extract(src: String): ExtractorResult =
    CommentExtractor.extract(src)

  @Test def simple_directive_line(): Unit = {
    val r = extract(
      """//> using scala 3
        |""".stripMargin
    )
    assertEquals(1, r.directiveLines.length)
    assertEquals(0, r.directiveLines.head.lineNum)
    assertEquals(18, r.codeOffset)
  }

  @Test def shebang_is_skipped_on_line_0(): Unit = {
    val src =
      """#!/usr/bin/env scala
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(1, r.directiveLines.head.lineNum)
  }

  @Test def shebang_after_line_0_treated_as_code(): Unit = {
    val src =
      """//> using scala 3
        |#!/usr/bin/env scala
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(18, r.codeOffset) // shebang line is code
  }

  @Test def blank_lines_do_not_affect_directive_region(): Unit = {
    val src =
      """
        |//> using scala 3
        |
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
  }

  @Test def line_comments_are_skipped(): Unit = {
    val src =
      """// a comment
        |//> using scala 3
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
  }

  @Test def block_comment_before_directive_is_skipped(): Unit = {
    val src =
      """/* comment */
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
  }

  @Test def multi_line_block_comment_is_skipped(): Unit = {
    val src =
      """/*
        | * block
        | */
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
  }

  @Test def codeOffset_points_to_start_of_first_code_line(): Unit = {
    val src =
      """//> using scala 3
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(18, r.codeOffset) // "//> using scala 3\n" is 18 chars
  }

  @Test def no_code_codeOffset_is_file_length(): Unit = {
    val src =
      """//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(src.length, r.codeOffset)
  }

  @Test def directive_after_code_is_ignored_with_warning(): Unit = {
    val src =
      """val x = 1
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertTrue(r.diagnostics.exists(_.severity == DiagnosticSeverity.Warning))
  }

  @Test def multiple_directives(): Unit = {
    val src =
      """//> using scala 3
        |//> using dep com.lihaoyi::os-lib:0.11.4
        |""".stripMargin
    val r = extract(src)
    assertEquals(2, r.directiveLines.length)
  }

  @Test def lineStartOffset_is_correct(): Unit = {
    val src =
      """//> using scala 3
        |//> using dep foo
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines(0).lineStartOffset)
    assertEquals(18, r.directiveLines(1).lineStartOffset)
  }

  @Test def directives_inside_block_comment_are_ignored(): Unit = {
    val src =
      """/* //> using scala 3 */
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
  }

  @Test def without_using_treated_as_code(): Unit = {
    val src =
      """//> notUsing foo
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(0, r.codeOffset) // first line is code
  }

  @Test def ScalaDoc_containing_directive_like_text_is_ignored(): Unit = {
    val src =
      """/** ScalaDoc '//> using scala 3' */
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def multi_line_ScalaDoc_containing_directive_like_text_is_ignored(): Unit = {
    val src =
      """/**
        | * //> using scala 3
        | */
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def line_comment_containing_embedded_using_text_is_skipped(): Unit = {
    val src =
      """// line comment '//> using scala 3'
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def issue_2382_all_three_comment_types_with_directive_like_text_produce_no_errors(): Unit = {
    val src =
      """// line comment '//> using ...'
        |/* block comment '//> using ...' */
        |/** ScalaDoc '//> using ...' */
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def block_comment_with_directive_like_text_followed_by_real_directive(): Unit = {
    val src =
      """/* //> using dep foo */
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(1, r.directiveLines.head.lineNum)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def package_statement_before_directive_makes_directive_post_code(): Unit = {
    val src =
      """package x
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(0, r.codeOffset)
    assertEquals(1, r.diagnostics.length)
    assertEquals(DiagnosticSeverity.Warning, r.diagnostics.head.severity)
    assertTrue(r.diagnostics.head.message.contains("Ignoring"))
    assertTrue(r.diagnostics.head.message.contains("//> using scala 3"))
  }

  @Test def multiple_directives_after_code_each_produce_a_warning(): Unit = {
    val src =
      """val x = 1
        |//> using scala 3
        |//> using dep foo
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    val ws = r.diagnostics.filter(_.severity == DiagnosticSeverity.Warning)
    assertEquals(2, ws.length)
    assertTrue(ws(0).message.contains("//> using scala 3"))
    assertTrue(ws(1).message.contains("//> using dep foo"))
  }

  @Test def post_code_directive_warning_has_correct_position(): Unit = {
    val src =
      """val x = 1
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    val w = r.diagnostics.head
    assertEquals(1, w.position.line)
    assertEquals(10, w.position.offset)
  }

  @Test def mix_of_valid_directives_and_post_code_directives(): Unit = {
    val src =
      """//> using scala 3
        |val x = 1
        |//> using dep foo
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(0, r.directiveLines.head.lineNum)
    val ws = r.diagnostics.filter(_.severity == DiagnosticSeverity.Warning)
    assertEquals(1, ws.length)
    assertTrue(ws.head.message.contains("//> using dep foo"))
  }

  @Test def leading_spaces_before_are_stripped_and_directive_is_parsed_correctly(): Unit = {
    val src =
      """  //> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    val dl = r.directiveLines.head
    assertEquals(dl.content, "//> using scala 3")
    assertEquals(2, dl.lineStartOffset)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def leading_tabs_before_are_stripped_and_directive_is_parsed_correctly(): Unit = {
    val src =
      """	//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(r.directiveLines.head.content, "//> using scala 3")
    assertEquals(0, r.diagnostics.length)
  }

  @Test def using_without_space_is_accepted(): Unit = {
    val src =
      """//>using scala 3
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(0, r.directiveLines.head.lineNum)
    assertEquals(17, r.codeOffset)
  }

  @Test def multiple_spaces_after_is_accepted(): Unit = {
    val src =
      """//>  using scala 3
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(0, r.directiveLines.head.lineNum)
    assertEquals(19, r.codeOffset)
  }

  @Test def tab_after_emits_a_warning(): Unit = {
    val src =
      """//>	using scala 3
        |val x = 1
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(0, r.directiveLines.head.lineNum)
    assertEquals(18, r.codeOffset)
  }

  @Test def CRLF_line_endings_parse_correctly(): Unit = {
    val src = "//> using scala 3\r\nval x = 1\r\n"
    val r   = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(r.directiveLines.head.content, "//> using scala 3\r")
    assertEquals(19, r.codeOffset) // includes \r\n
  }

  @Test def UTF_8_BOM_at_start_of_file_is_stripped_and_directives_are_parsed(): Unit = {
    val src =
      s"""\uFEFF//> using scala 3
         |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(src.length, r.codeOffset)
  }

  @Test def UTF_8_BOM_before_code_does_not_suppress_post_code_directive_warnings(): Unit = {
    val src =
      s"""\uFEFFval x = 1
         |//> using scala 3
         |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(1, r.codeOffset) // BOM(1) + start of `val`
    val ws = r.diagnostics.filter(_.severity == DiagnosticSeverity.Warning)
    assertEquals(1, ws.length)
    assertTrue(ws.head.message.contains("Ignoring"))
  }

  @Test def directive_on_same_line_after_block_comment_closing_is_treated_as_code(): Unit = {
    val src =
      """/* comment */ //> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(0, r.codeOffset)
  }

  @Test def unclosed_block_comment_swallows_rest_of_file_gracefully(): Unit = {
    val src =
      """/* never closed
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(0, r.directiveLines.length)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def nested_block_comments_are_handled_correctly(): Unit = {
    val src =
      """/* outer /* inner */ still comment */
        |//> using scala 3
        |""".stripMargin
    val r = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(1, r.directiveLines.head.lineNum)
  }

  @Test def directive_at_end_of_file_without_trailing_newline(): Unit = {
    val src = "//> using scala 3"
    val r   = extract(src)
    assertEquals(1, r.directiveLines.length)
    assertEquals(src.length, r.codeOffset)
    assertEquals(0, r.diagnostics.length)
  }
