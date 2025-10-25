// Test error cases for dedented string literals

object DedentedStringErrors {
  // Error: No newline after opening quotes
  val noNewlineAfterOpen = '''content on same line // error: dedented string literal must start with a newline

  // Error: Content not indented enough
  val notIndented = '''
content // error: line in dedented string literal is indented less than the closing delimiter
  '''

  // Error: Mixed tabs and spaces - first line has tab, but closing delimiter has spaces
  val mixedTabsSpaces = '''
	tab line // error: line in dedented string literal is indented less than the closing delimiter
  space line
	'''

  // Error: Non-whitespace before closing delimiter
  val nonWhitespaceBeforeClosing = '''
  content here
  text''' // error: last line of dedented string literal must contain only whitespace before closing delimiter
}
















// Error: Unclosed literal - must be last since it breaks parsing
object UnclosedTest {
  val unclosed = '''
  some content // error: unclosed dedented string literal
