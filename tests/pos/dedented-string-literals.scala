// Test dedented string literals as specified in SIP

object DedentedStringLiterals {
  // Basic usage
  val basic = '''
i am cow
hear me moo
'''

  // With indentation preserved
  val withIndent = '''
  i am cow
  hear me moo
'''

  // Empty string
  val empty = '''
'''

  // Single line of content
  val singleLine = '''
hello world
'''

  // Multiple blank lines
  val blankLines = '''
line 1

line 3
'''

  // Deep indentation
  val deepIndent = '''
      deeply
      indented
      content
'''

  // Mixed content indentation (more than closing)
  val mixedIndent = '''
  first level
    second level
      third level
'''

  // Using extended delimiter to include '''
  val withTripleQuotes = ''''
'''
i am cow
hear me moo
'''
''''

  // Extended delimiter with 5 quotes
  val extended5 = '''''
''''
content with four quotes
''''
'''''

  // Tabs for indentation
  val withTabs = '''
	tab indented
	content here
'''

  // Empty lines are allowed anywhere
  val emptyLinesAnywhere = '''

content

more content

'''

  // Testing in different contexts
  def inFunction = '''
  function content
  more content
'''

  class InClass {
    val inClass = '''
    class member
    content
'''
  }

  // In a list
  val list = List(
    '''
    first
    ''',
    '''
    second
    ''',
    '''
    third
    '''
  )

  // Nested in expressions
  val nested = "prefix" + '''
  middle
  ''' + "suffix"

  // With special characters
  val specialChars = '''
!"#$%&()*+,-./:;<=>?@[\]^_`{|}~
'''

  // Unicode content
  val unicode = '''
Hello 世界
Καλημέρα κόσμε
'''

  // Zero-width closing indent
  val zeroIndent = '''
content
'''

  // Content with quotes
  val withQuotes = '''
"double quotes"
'single quote'
''
'''
}
