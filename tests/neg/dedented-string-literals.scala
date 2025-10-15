// Test error cases for dedented string literals

object DedentedStringErrors {
  // Error: No newline after opening quotes
  val noNewlineAfterOpen = '''content on same line // error

  // Error: Content not indented enough // error
  val notIndented = '''
content
  '''

  // Error: Mixed tabs and spaces // error
  val mixedTabsSpaces = '''
	tab line
  space line
	'''

  // Error: Unclosed literal // error
  val unclosed = '''
some content
}
