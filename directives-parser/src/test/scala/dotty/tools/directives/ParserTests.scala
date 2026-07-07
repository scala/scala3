package dotty.tools.directives

import org.junit.Test
import org.junit.Assert.*

class ParserTests:

  private def parse(src: String): UsingDirectivesResult =
    UsingDirectivesParser.parse(src)

  private def directives(src: String): Seq[UsingDirective] =
    parse(src).directives

  private def warnings(src: String): Seq[UsingDirectiveDiagnostic] =
    parse(src).diagnostics.filter(_.severity == DiagnosticSeverity.Warning)

  // -----------------------------------------------------------------------
  // Basic parsing
  // -----------------------------------------------------------------------

  @Test def parse_simple_directive_key_and_value(): Unit = {
    val ds = directives(
      """//> using scala 3
        |""".stripMargin
    )
    assertEquals(1, ds.length)
    assertEquals("scala", ds.head.key)
    assertEquals(1, ds.head.values.length)
    val v = ds.head.values.head
    v match
      case sv: DirectiveValue.StringVal => assertEquals("3", sv.value)
      case _                            => fail(s"expected StringVal, got $v")
  }

  @Test def parse_dotted_key(): Unit = {
    val ds = directives(
      """//> using test.dep munit::munit:1.0.0
        |""".stripMargin
    )
    assertEquals("test.dep", ds.head.key)
  }

  @Test def parse_multiple_values(): Unit = {
    val ds = directives(
      """//> using dep com.lihaoyi::os-lib:0.11.4 com.lihaoyi::upickle:3.1.0
        |""".stripMargin
    )
    val values = ds.head.values
    assertEquals(2, values.length)
  }

  @Test def parse_quoted_string_value(): Unit = {
    val src =
      """//> using scalacOption "-Xfatal-warnings"
        |""".stripMargin
    val ds = directives(src)
    val v  = ds.head.values.head
    v match
      case sv: DirectiveValue.StringVal =>
        assertEquals("-Xfatal-warnings", sv.value)
        assertTrue(sv.isQuoted)
      case _ => fail(s"expected StringVal, got $v")
  }

  @Test def parse_boolean_true(): Unit = {
    val ds = directives(
      """//> using publish.doc true
        |""".stripMargin
    )
    val v = ds.head.values.head
    v match
      case bv: DirectiveValue.BoolVal => assertEquals(true, bv.value)
      case _                          => fail(s"expected BoolVal, got $v")
  }

  @Test def parse_boolean_false(): Unit = {
    val ds = directives(
      """//> using publish.doc false
        |""".stripMargin
    )
    val v = ds.head.values.head
    v match
      case bv: DirectiveValue.BoolVal => assertEquals(false, bv.value)
      case _                          => fail(s"expected BoolVal, got $v")
  }

  @Test def directive_with_no_values_produces_EmptyVal(): Unit = {
    val ds = directives(
      """//> using toolkit
        |""".stripMargin
    )
    assertEquals(1, ds.head.values.length)
    ds.head.values.head match
      case _: DirectiveValue.EmptyVal => ()
      case v                          => fail(s"expected EmptyVal, got $v")
  }

  @Test def multiple_directives(): Unit = {
    val src =
      """//> using scala 3
        |//> using dep foo
        |""".stripMargin
    val ds = directives(src)
    assertEquals(2, ds.length)
    assertEquals("scala", ds(0).key)
    assertEquals("dep", ds(1).key)
  }

  // -----------------------------------------------------------------------
  // Comma deprecation
  // -----------------------------------------------------------------------

  @Test def comma_separator_emits_deprecation_warning(): Unit = {
    val ws = warnings(
      """//> using dep a, b
        |""".stripMargin
    )
    assertTrue(s"expected warnings, got none", ws.nonEmpty)
    assertTrue(ws.exists(_.message.contains("deprecated")))
  }

  @Test def each_comma_as_separator_emits_its_own_deprecation_warning(): Unit = {
    val ws = warnings(
      """//> using dep a, b, c
        |""".stripMargin
    )
    val deprecationMsgs = ws.filter(_.message.contains("Use of commas as separators"))
    assertEquals(2, deprecationMsgs.length)
  }

  @Test def comma_separator_still_parses_both_values(): Unit = {
    val ds = directives(
      """//> using dep a, b
        |""".stripMargin
    )
    assertEquals(2, ds.head.values.length)
  }

  @Test def embedded_comma_no_space_does_NOT_emit_warning(): Unit = {
    val ws = warnings(
      """//> using packaging.graalvmArgs --enable-url-protocols=http,https
        |""".stripMargin
    )
    assertEquals(0, ws.length)
  }

  // -----------------------------------------------------------------------
  // Position tracking
  // -----------------------------------------------------------------------

  @Test def positions_are_correct_for_single_directive(): Unit = {
    // "//> using scala 3"
    //  0123456789012345678
    //            ^key  ^value
    val ds = directives(
      """//> using scala 3
        |""".stripMargin
    )
    val kp = ds.head.keyPosition
    assertEquals(0, kp.line)
    assertEquals(10, kp.column)
    assertEquals(10, kp.offset)
    val vp = ds.head.values.head.position
    assertEquals(16, vp.column)
  }

  @Test def positions_are_correct_for_second_directive_on_next_line(): Unit = {
    val src =
      """//> using scala 3
        |//> using dep foo
        |""".stripMargin
    val ds = directives(src)
    val kp = ds(1).keyPosition
    assertEquals(1, kp.line)
    // "//> using scala 3\n" is 18 chars, then "//> using " is 10 more = offset 28
    assertEquals(28, kp.offset)
  }

  // -----------------------------------------------------------------------
  // Diagnostics
  // -----------------------------------------------------------------------

  @Test def directive_after_code_is_ignored_with_warning(): Unit = {
    val src =
      """val x = 1
        |//> using scala 3
        |""".stripMargin
    val r = parse(src)
    assertEquals(0, r.directives.length)
    assertTrue(r.diagnostics.exists(_.severity == DiagnosticSeverity.Warning))
  }

  // -----------------------------------------------------------------------
  // codeOffset
  // -----------------------------------------------------------------------

  @Test def codeOffset_after_single_directive(): Unit = {
    val src =
      """//> using scala 3
        |val x = 1
        |""".stripMargin
    val r = parse(src)
    assertEquals(18, r.codeOffset)
  }

  @Test def codeOffset_with_no_directives_is_0(): Unit = {
    val src =
      """val x = 1
        |""".stripMargin
    val r = parse(src)
    assertEquals(0, r.codeOffset)
  }

  @Test def codeOffset_at_end_of_file_when_only_directives(): Unit = {
    val src =
      """//> using scala 3
        |""".stripMargin
    val r = parse(src)
    assertEquals(src.length, r.codeOffset)
  }

  // -----------------------------------------------------------------------
  // Edge cases
  // -----------------------------------------------------------------------

  @Test def empty_source(): Unit = {
    val r = parse("")
    assertEquals(0, r.directives.length)
    assertEquals(0, r.codeOffset)
  }

  @Test def source_with_only_blank_lines(): Unit = {
    val r = parse("\n\n\n")
    assertEquals(0, r.directives.length)
  }

  @Test def value_containing_colon(): Unit = {
    val ds = directives(
      """//> using dep com.lihaoyi::os-lib:0.11.4
        |""".stripMargin
    )
    ds.head.values.head match
      case sv: DirectiveValue.StringVal =>
        assertEquals("com.lihaoyi::os-lib:0.11.4", sv.value)
      case v => fail(s"expected StringVal, got $v")
  }

  @Test def directive_like_text_in_all_comment_types_produces_no_directives_or_errors(): Unit = {
    val src =
      """// line comment '//> using ...'
        |/* block comment '//> using ...' */
        |/** ScalaDoc '//> using ...' */
        |""".stripMargin
    val r = parse(src)
    assertEquals(0, r.directives.length)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def ScalaDoc_before_real_directive_does_not_interfere(): Unit = {
    val src =
      """/** //> using dep fake */
        |//> using scala 3
        |val x = 1
        |""".stripMargin
    val r = parse(src)
    assertEquals(1, r.directives.length)
    assertEquals("scala", r.directives.head.key)
    assertEquals(0, r.diagnostics.length)
  }

  @Test def package_before_directives_causes_directives_to_be_ignored_with_warning(): Unit = {
    val src =
      """package x
        |//> using scala 3.4.2
        |//> using dep foo
        |@main def run = println(42)
        |""".stripMargin
    val r = parse(src)
    assertEquals(0, r.directives.length)
    assertEquals(0, r.codeOffset)
    val ws = r.diagnostics.filter(_.severity == DiagnosticSeverity.Warning)
    assertEquals(2, ws.length)
    assertTrue(ws(0).message.contains("//> using scala 3.4.2"))
    assertTrue(ws(1).message.contains("//> using dep foo"))
  }

  @Test def post_code_directive_warning_includes_directive_text(): Unit = {
    val src =
      """val x = 1
        |//> using scala 3
        |""".stripMargin
    val ws = warnings(src)
    assertEquals(1, ws.length)
    assertTrue(ws.head.message.contains("Ignoring using directive found after Scala code"))
    assertTrue(ws.head.message.contains("//> using scala 3"))
  }

  @Test def indented_directive_is_parsed_correctly_end_to_end(): Unit = {
    val src =
      """  //> using scala 3
        |val x = 1
        |""".stripMargin
    val r = parse(src)
    assertEquals(1, r.directives.length)
    assertEquals("scala", r.directives.head.key)
    val v = r.directives.head.values.head
    v match
      case sv: DirectiveValue.StringVal => assertEquals("3", sv.value)
      case other                        => fail(s"expected StringVal, got $other")
  }

  @Test def inline_block_comment_in_directive_line_is_not_supported_key_becomes(): Unit = {
    val src =
      """//> using /* comment */ dep foo
        |""".stripMargin
    val r = parse(src)
    assertEquals(1, r.directives.length)
    assertEquals("/*", r.directives.head.key)
  }

  private def errors(src: String): Seq[UsingDirectiveDiagnostic] =
    parse(src).diagnostics.filter(_.severity == DiagnosticSeverity.Error)

  @Test def using_alone_emits_error_about_missing_key(): Unit = {
    val errs = errors(
      """//> using
        |""".stripMargin
    )
    assertEquals(1, errs.length)
    assertTrue(errs.head.message.contains("Expected a key after `using`"))
  }

  @Test def using_as_key_emits_error(): Unit = {
    val errs = errors(
      """//> using using foo
        |""".stripMargin
    )
    assertEquals(1, errs.length)
    assertTrue(errs.head.message.contains("Expected a key after `using`"))
    assertTrue(errs.head.message.contains("Using"))
  }

  @Test def true_as_key_emits_error(): Unit = {
    val errs = errors(
      """//> using true foo
        |""".stripMargin
    )
    assertEquals(1, errs.length)
    assertTrue(errs.head.message.contains("Expected a key after `using`"))
    assertTrue(errs.head.message.contains("BoolLit"))
  }

  @Test def false_as_key_emits_error(): Unit = {
    val errs = errors(
      """//> using false foo
        |""".stripMargin
    )
    assertEquals(1, errs.length)
    assertTrue(errs.head.message.contains("Expected a key after `using`"))
  }

  @Test def key_with_trailing_dot_is_accepted_by_parser(): Unit = {
    val ds = directives(
      """//> using foo. bar
        |""".stripMargin
    )
    assertEquals(1, ds.length)
    assertEquals("foo.", ds.head.key)
  }

  @Test def key_with_leading_dot_is_accepted_by_parser(): Unit = {
    val ds = directives(
      """//> using .foo bar
        |""".stripMargin
    )
    assertEquals(1, ds.length)
    assertEquals(".foo", ds.head.key)
  }

  @Test def comma_with_no_spaces_is_a_single_value_no_warnings(): Unit = {
    val src =
      """//> using dep a,b
        |""".stripMargin
    val ds = directives(src)
    assertEquals(1, ds.head.values.length)
    ds.head.values.head match
      case sv: DirectiveValue.StringVal => assertEquals("a,b", sv.value)
      case v                            => fail(s"expected StringVal, got $v")
    assertEquals(0, warnings(src).length)
  }

  @Test def space_before_comma_but_not_after_produces_two_values(): Unit = {
    val src =
      """//> using dep a ,b
        |""".stripMargin
    val ds = directives(src)
    assertEquals(2, ds.head.values.length)
    val vs = ds.head.values.collect { case sv: DirectiveValue.StringVal => sv.value }
    assertEquals(Seq("a", ",b"), vs)
    assertEquals(0, warnings(src).length)
  }

  @Test def trailing_comma_produces_2_values_and_2_deprecation_warnings(): Unit = {
    val src =
      """//> using dep a, b,
        |""".stripMargin
    val ds = directives(src)
    assertEquals(2, ds.head.values.length)
    val vs = ds.head.values.collect { case sv: DirectiveValue.StringVal => sv.value }
    assertEquals(Seq("a", "b"), vs)
    val ws = warnings(src)
    assertEquals(2, ws.length)
    assertTrue(ws.forall(_.message.contains("deprecated")))
  }

  @Test def lone_comma_is_treated_as_a_literal_value(): Unit = {
    val src =
      """//> using dep ,
        |""".stripMargin
    val ds = directives(src)
    assertEquals(1, ds.head.values.length)
    ds.head.values.head match
      case sv: DirectiveValue.StringVal => assertEquals(",", sv.value)
      case v                            => fail(s"expected StringVal(','), got $v")
    assertEquals(0, warnings(src).length)
  }

  @Test def double_comma_emits_warning_about_value_ending_with_comma(): Unit = {
    val src =
      """//> using dep a,, b
        |""".stripMargin
    val ds = directives(src)
    val vs = ds.head.values.collect { case sv: DirectiveValue.StringVal => sv.value }
    assertEquals(Seq("a,", "b"), vs)
    val ws = warnings(src)
    assertTrue(s"warnings=$ws", ws.exists(_.message.contains("ends with a comma")))
    assertTrue(s"warnings=$ws", ws.exists(_.message.contains("deprecated")))
  }

  @Test def comma_inside_quoted_string_is_preserved_literally(): Unit = {
    val src =
      """//> using dep "a,b"
        |""".stripMargin
    val ds = directives(src)
    assertEquals(1, ds.head.values.length)
    ds.head.values.head match
      case sv: DirectiveValue.StringVal =>
        assertEquals("a,b", sv.value)
        assertTrue(sv.isQuoted)
      case v => fail(s"expected quoted StringVal, got $v")
    assertEquals(0, warnings(src).length)
  }

  @Test def coursier_dep_with_url_is_a_single_value(): Unit = {
    val src =
      """//> using dep tabby:tabby:0.2.3,url=https://example.com/tabby.jar
        |""".stripMargin
    val ds = directives(src)
    assertEquals(1, ds.head.values.length)
    ds.head.values.head match
      case sv: DirectiveValue.StringVal =>
        assertEquals("tabby:tabby:0.2.3,url=https://example.com/tabby.jar", sv.value)
      case v => fail(s"expected StringVal, got $v")
    assertEquals(0, warnings(src).length)
  }

  @Test def coursier_dep_with_exclude_is_a_single_value(): Unit = {
    val src =
      """//> using dep com.lihaoyi::os-lib:0.11.3,exclude=com.lihaoyi%%geny
        |""".stripMargin
    val ds = directives(src)
    assertEquals(1, ds.head.values.length)
    ds.head.values.head match
      case sv: DirectiveValue.StringVal =>
        assertEquals("com.lihaoyi::os-lib:0.11.3,exclude=com.lihaoyi%%geny", sv.value)
      case v => fail(s"expected StringVal, got $v")
    assertEquals(0, warnings(src).length)
  }

  @Test def mixed_commas_some_as_separators_some_embedded(): Unit = {
    val src =
      """//> using dep a,b, c,d
        |""".stripMargin
    val ds = directives(src)
    val vs = ds.head.values.collect { case sv: DirectiveValue.StringVal => sv.value }
    assertEquals(Seq("a,b", "c,d"), vs)
    val ws = warnings(src)
    assertEquals(1, ws.count(_.message.contains("deprecated")))
  }

  @Test def quoted_values_with_comma_separator_between_them(): Unit = {
    val src =
      """//> using javacOpt "source", "1.8"
        |""".stripMargin
    val ds = directives(src)
    assertEquals(2, ds.head.values.length)
    val vs = ds.head.values.collect { case sv: DirectiveValue.StringVal => sv.value }
    assertEquals(Seq("source", "1.8"), vs)
    assertEquals(1, warnings(src).count(_.message.contains("deprecated")))
  }

  @Test def bare_value_touching_quoted_string_emits_error(): Unit = {
    val src =
      """//> using dep a,"b"
        |""".stripMargin
    val r    = parse(src)
    val errs = r.diagnostics.filter(_.severity == DiagnosticSeverity.Error)
    assertTrue(s"errors=$errs", errs.exists(_.message.contains("Whitespace is required")))
  }
