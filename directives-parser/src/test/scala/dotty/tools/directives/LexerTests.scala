package dotty.tools.directives

import org.junit.Test
import org.junit.Assert.*

class LexerTests:

  private def lex(line: String, lineNum: Int = 0): Seq[Token] =
    Lexer.tokenize(line, lineNum, 0)

  private def lexTokens(line: String): Seq[Token] =
    lex(line).dropRight(1) // drop trailing Newline

  @Test def using_is_tokenized_as_Using_not_Ident(): Unit = {
    val tokens = lexTokens("//> using scala 3")
    assertTrue(s"got ${tokens.head}", tokens.head match { case _: Token.Using => true; case _ => false })
  }

  @Test def tokenize_bare_identifiers(): Unit = {
    val tokens = lexTokens("//> using dep foo")
    val idents = tokens.collect { case Token.Ident(v, _) => v }
    assertEquals(Seq("dep", "foo"), idents)
  }

  @Test def tokenize_dotted_key_as_single_Ident(): Unit = {
    val tokens = lexTokens("//> using test.dep munit")
    val idents = tokens.collect { case Token.Ident(v, _) => v }
    assertEquals(Seq("test.dep", "munit"), idents)
  }

  @Test def tokenize_quoted_string(): Unit = {
    val tokens  = lexTokens("""//> using dep "com.lihaoyi::os-lib:0.11.4"""")
    val strLits = tokens.collect { case Token.StringLit(v, _) => v }
    assertEquals(Seq("com.lihaoyi::os-lib:0.11.4"), strLits)
  }

  @Test def tokenize_boolean_true(): Unit = {
    val tokens = lexTokens("//> using publish.doc true")
    assertTrue(tokens.exists { case Token.BoolLit(true, _) => true; case _ => false })
  }

  @Test def tokenize_boolean_false(): Unit = {
    val tokens = lexTokens("//> using publish.doc false")
    assertTrue(tokens.exists { case Token.BoolLit(false, _) => true; case _ => false })
  }

  @Test def comma_followed_by_whitespace_is_a_Comma_token(): Unit = {
    val tokens = lexTokens("//> using dep a, b")
    assertTrue(tokens.exists { case _: Token.Comma => true; case _ => false })
    val idents = tokens.collect { case Token.Ident(v, _) => v }
    assertEquals(Seq("a", "b"), idents.filter(_ != "dep"))
  }

  @Test def comma_embedded_in_value_is_NOT_a_separator(): Unit = {
    val tokens = lexTokens("//> using packaging.graalvmArgs --enable-url-protocols=http,https")
    val idents = tokens.collect { case Token.Ident(v, _) => v }
    assertTrue(s"idents=$idents", idents.contains("--enable-url-protocols=http,https"))
    assertTrue(!tokens.exists { case _: Token.Comma => true; case _ => false })
  }

  @Test def unterminated_string_literal_produces_LexError(): Unit = {
    val tokens = lexTokens("""//> using dep "unterminated""")
    assertTrue(s"tokens=$tokens", tokens.exists { case _: Token.LexError => true; case _ => false })
  }

  @Test def string_escape_sequences(): Unit = {
    val tokens  = lexTokens("""//> using dep "line1\nline2"""")
    val strLits = tokens.collect { case Token.StringLit(v, _) => v }
    assertEquals(Seq("line1\nline2"), strLits)
  }

  @Test def column_positions_of_key_and_value_are_correct(): Unit = {
    // "//> using dep foo"
    //            ^10   ^14
    val tokens = lexTokens("//> using dep foo")
    val depPos = tokens.collectFirst { case Token.Ident("dep", p) => p }.get
    assertEquals(10, depPos.column)
    val fooPos = tokens.collectFirst { case Token.Ident("foo", p) => p }.get
    assertEquals(14, fooPos.column)
  }

  @Test def line_position_is_preserved(): Unit = {
    val tokens = Lexer.tokenize("//> using dep foo", lineNum = 3, lineStartOffset = 100)
    val depPos = tokens.collectFirst { case Token.Ident("dep", p) => p }.get
    assertEquals(3, depPos.line)
  }

  @Test def trailing_Newline_token_is_always_emitted(): Unit = {
    val tokens = lex("//> using scala 3")
    assertTrue(tokens.last match { case _: Token.Newline => true; case _ => false })
  }

  @Test def empty_directive_line_still_produces_Newline(): Unit = {
    val tokens = lex("//> using")
    assertTrue(tokens.last match { case _: Token.Newline => true; case _ => false })
  }

  @Test def backtick_quoted_identifier_strips_backticks(): Unit = {
    val tokens1 = lexTokens("//> using `native-gc`")
    assertEquals(Seq("native-gc"), tokens1.collect { case Token.Ident(v, _) => v })

    val tokens2 = lexTokens("//> using `native-mode`")
    assertEquals(Seq("native-mode"), tokens2.collect { case Token.Ident(v, _) => v })
  }

  @Test def invalid_unicode_escape_produces_LexError_instead_of_crashing(): Unit = {
    val tokens = lexTokens("//> using dep \"\\uZZZZ\"")
    assertTrue(s"tokens=$tokens", tokens.exists { case _: Token.LexError => true; case _ => false })
    val err = tokens.collectFirst { case Token.LexError(msg, _) => msg }.get
    assertTrue(s"error message: $err", err.contains("Invalid unicode escape"))
  }

  @Test def empty_backtick_identifier_produces_LexError(): Unit = {
    val tokens = lexTokens("//> using `` foo")
    assertTrue(s"tokens=$tokens", tokens.exists { case _: Token.LexError => true; case _ => false })
    val err = tokens.collectFirst { case Token.LexError(msg, _) => msg }.get
    assertTrue(s"error message: $err", err.contains("Empty backtick identifier"))
  }

  @Test def empty_quoted_string_produces_StringLit_with_empty_value(): Unit = {
    val tokens = lexTokens("//> using dep \"\"")
    val strs   = tokens.collect { case Token.StringLit(v, _) => v }
    assertEquals(Seq(""), strs)
  }

  @Test def backtick_quoted_value_is_parsed_as_bare_Ident(): Unit = {
    val tokens = lexTokens("//> using dep `com.lihaoyi::os-lib:0.11.4`")
    val idents = tokens.collect { case Token.Ident(v, _) => v }
    assertTrue(s"idents=$idents", idents.contains("com.lihaoyi::os-lib:0.11.4"))
  }

  @Test def comma_with_no_spaces_produces_single_token(): Unit = {
    val tokens = lexTokens("//> using dep a,b")
    val idents = tokens.collect { case Token.Ident(v, _) => v }
    assertEquals(Seq("a,b"), idents.filter(_ != "dep"))
    assertTrue("should not produce Comma token", !tokens.exists { case _: Token.Comma => true; case _ => false })
  }

  @Test def space_before_comma_but_not_after_produces_two_tokens(): Unit = {
    val tokens = lexTokens("//> using dep a ,b")
    val idents = tokens.collect { case Token.Ident(v, _) => v }
    assertEquals(Seq("a", ",b"), idents.filter(_ != "dep"))
    assertTrue("should not produce Comma token", !tokens.exists { case _: Token.Comma => true; case _ => false })
  }

  @Test def double_comma_produces_bare_token_ending_with_comma_then_Comma_separator(): Unit = {
    val tokens = lexTokens("//> using dep a,, b")
    val idents = tokens.collect { case Token.Ident(v, _) => v }
    assertEquals(Seq("a,", "b"), idents.filter(_ != "dep"))
    assertTrue("second comma should be a separator", tokens.exists { case _: Token.Comma => true; case _ => false })
  }

  @Test def bare_value_touching_quoted_string_emits_LexError(): Unit = {
    val tokens = lexTokens("""//> using dep a,"b"""")
    assertTrue(s"tokens=$tokens", tokens.exists { case _: Token.LexError => true; case _ => false })
    val err = tokens.collectFirst { case Token.LexError(msg, _) => msg }.get
    assertTrue(s"error message: $err", err.contains("Whitespace is required"))
  }
