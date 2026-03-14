// Test error cases for dedented string literals

object DedentedStringErrors {
  val noNewlineAfterOpen = '''content on same line // error: dedented string literal must start with newline after opening quotes

  val notIndentedEnough = '''
content // error: line in dedented string literal must be indented at least as much as the closing delimiter with an identical prefix
  '''

  val mixedTabsSpaces = '''
	tab line // error: line in dedented string literal must be indented at least as much as the closing delimiter with an identical prefix
  space line
  '''

  val nonWhitespaceBeforeClosing = '''
  content here
  text''' // error: last line of dedented string literal must contain only whitespace before closing delimiter
}

object UnclosedTest {
  val unclosed = ''' // error: unclosed dedented string literal
  some content
