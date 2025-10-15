// Test error cases for dedented string literals

object DedentedStringErrors { // nopos-error // nopos-error // nopos-error
  // Error: No newline after opening quotes
  val noNewlineAfterOpen = '''content on same line // error

  // Error: Content not indented enough
  val notIndented = '''
content
  '''

  // Error: Mixed tabs and spaces
  val mixedTabsSpaces = '''
	tab line
  space line
	'''

  // Error: Unclosed literal
  val unclosed = '''
some content

  // Error: Non-whitespace before closing delimiter
  val nonWhitespaceBeforeClosing = '''
  content here
  text'''
}

// Test @compileTimeOnly with dedented string
object CompileTimeOnlyTest {
  import scala.annotation.compileTimeOnly

  @compileTimeOnly('''
  This method should only be used at compile time
  Do not call at runtime
  ''')
  def onlyAtCompileTime: Unit = ()

  def test(): Unit = {
    onlyAtCompileTime // error
  }
}
