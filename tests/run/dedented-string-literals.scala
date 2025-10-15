// Test runtime behavior of dedented string literals

object Test {
  def main(args: Array[String]): Unit = {
    // Test basic dedenting
    val basic = '''
    i am cow
    hear me moo
    '''
    println("Basic:")
    println(basic)
    println()

    // Test with indentation preserved
    val withIndent = '''
      i am cow
      hear me moo
    '''
    println("With indent:")
    println(withIndent)
    println()

    // Test empty string
    val empty = '''
    '''
    println("Empty:")
    println(s"[${empty}]")
    println()

    // Test single line
    val singleLine = '''
    hello world
    '''
    println("Single line:")
    println(singleLine)
    println()

    // Test blank lines
    val blankLines = '''
    line 1

    line 3
    '''
    println("Blank lines:")
    println(blankLines)
    println()

    // Test deep indentation removal
    val deepIndent = '''
          deeply
          indented
          content
    '''
    println("Deep indent:")
    println(deepIndent)
    println()

    // Test mixed indentation levels (preserved)
    val mixedIndent = '''
      first level
        second level
          third level
    '''
    println("Mixed indent:")
    println(mixedIndent)
    println()

    // Test extended delimiter with embedded '''
    val withTripleQuotes = ''''
    '''
    i am cow
    '''
    ''''
    println("With triple quotes:")
    println(withTripleQuotes)
    println()

    // Test extended delimiter with 5 quotes
    val extended5 = '''''
    ''''
    content with four quotes
    ''''
    '''''
    println("Extended 5 quotes:")
    println(extended5)
    println()

    // Test that newlines are normalized to \n
    val normalized = '''
    line1
    line2
    '''
    println("Normalized newlines:")
    println(s"Has only LF: ${!normalized.contains('\r')}")
    println()

    // Test special characters
    val specialChars = '''
    !"#$%&()*+,-./:;<=>?@[\]^_`{|}~
    '''
    println("Special chars:")
    println(specialChars)
    println()

    // Test unicode
    val unicode = '''
    Hello 世界
    '''
    println("Unicode:")
    println(unicode)
    println()

    // Test tabs for indentation
    val withTabs = '''
		tab indented
		content here
	'''
    println("With tabs:")
    println(withTabs)
    println()

    // Test empty lines anywhere
    val emptyLinesAnywhere = '''

    content

    more content

    '''
    println("Empty lines anywhere:")
    println(s"[${emptyLinesAnywhere}]")
    println()

    // Test content with quotes
    val withQuotes = '''
    "double quotes"
    'single quote'
    ''
    '''
    println("With quotes:")
    println(withQuotes)
    println()

    // Test zero-width closing indent
    val zeroIndent = '''
    content
    '''
    println("Zero indent:")
    println(zeroIndent)
    println()

    // Test content length and character accuracy
    val precise = '''
      ab
      cd
    '''
    println("Precise:")
    println(s"Length: ${precise.length}")
    println(s"Content: [${precise}]")
    println(s"Chars: ${precise.toList}")
    println()

    // Test with string interpolator
    val name = "Alice"
    val age = 30
    val interpolated = s'''
    Hello $name
    You are $age years old
    '''
    println("Interpolated:")
    println(interpolated)
    println()

    // Test with f interpolator
    val value = 42
    val formatted = f'''
    Value: $value%05d
    Done
    '''
    println("Formatted:")
    println(formatted)
    println()

    // Test as pattern
    def testPattern(s: String): String = s match {
      case '''
      test
      ''' => "matched basic"
      case '''
      other
      ''' => "matched other"
      case _ => "no match"
    }
    println("Pattern matching:")
    println(s"Pattern result: ${testPattern("test")}")
    println()

    // Test as pattern with interpolator
    def testInterpolatedPattern(s: String): String = s match {
      case s'''
      Hello $_
      ''' => "matched greeting"
      case _ => "no match"
    }
    println("Interpolated pattern:")
    println(s"Interpolated pattern result: ${testInterpolatedPattern("Hello World")}")
    println()

    // Test as pattern with two lines
    def testPatternTwoLines(s: String): String = s match {
      case '''
      line one
      line two
      ''' => "matched two lines"
      case _ => "no match"
    }
    println("Pattern matching (two lines):")
    println(s"Two line pattern result: ${testPatternTwoLines("line one\nline two")}")
    println()

    // Test as pattern with interpolator and two lines
    def testInterpolatedPatternTwoLines(s: String): String = s match {
      case s'''
      First: $_
      Second: $_
      ''' => "matched two line greeting"
      case _ => "no match"
    }
    println("Interpolated pattern (two lines):")
    println(s"Two line interpolated result: ${testInterpolatedPatternTwoLines("First: Alice\nSecond: Bob")}")
    println()

    // Test in function context
    def inFunction = '''
      function content
      more content
    '''
    println("In function:")
    println(inFunction)
    println()

    // Test in class context
    class InClass {
      val inClass = '''
        class member
        content
      '''
    }
    val classInstance = new InClass
    println("In class:")
    println(classInstance.inClass)
    println()

    // Test in a list
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
    println("In list:")
    list.foreach { item =>
      println(s"Item: [$item]")
    }
    println()

    // Test nested in expressions
    val nested = "prefix" + '''
      middle
    ''' + "suffix"
    println("Nested in expressions:")
    println(nested)
    println()

    // Test as type ascription (singleton literal type)
    val typedVal: '''
      first line
        indented line
      third line
    ''' = "  first line\n    indented line\n  third line"
    println("Type ascription:")
    println(s"Value: [$typedVal]")
    println(s"Type matches: ${typedVal == "  first line\n    indented line\n  third line"}")
    println()

    // Test as type parameter to valueOf
    val valueOfResult = scala.compiletime.constValue['''
      alpha
        beta
      gamma
    ''']
    println("valueOf test:")
    println(s"Value: [$valueOfResult]")
    println(s"Type matches: ${valueOfResult == "  alpha\n    beta\n  gamma"}")
  }
}
