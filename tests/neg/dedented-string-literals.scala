// Test error cases for dedented string literals

object DedentedStringErrors {
  // Error: No newline after opening quotes
  val noNewlineAfterOpen = '''content on same line // error

  // Error: Content not indented enough
  val notIndented = '''
content
  ''' // error

  // Error: Mixed tabs and spaces
  val mixedTabsSpaces = '''
	tab line
  space line
	''' // error

  // Error: Unclosed literal
  val unclosed = '''
some content
  // error: missing closing quotes
}
