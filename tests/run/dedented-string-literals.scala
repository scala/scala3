// Test runtime behavior of dedented string literals

object Test {
  def main(args: Array[String]): Unit = {
    val basic = '''
    i am cow
    hear me moo
    '''
    println("Basic:")
    println(basic)
    println("----")

    val noIndent = '''
i am cow
hear me moo
'''
    println("No Indent:")
    println(noIndent)
    println("----")

    val withIndentPreserved = '''
      i am cow
      hear me moo
    '''
    println("With indent:")
    println(withIndentPreserved)
    println("----")

    val empty = '''
    '''
    println("Empty:")
    println(s"[${empty}]")
    println("----")

    val singleLine = '''
    hello world
    '''
    println("Single line:")
    println(singleLine)
    println("----")

    val blankLines = '''
    line 1

    line 3
    '''
    println("Blank lines:")
    println(blankLines)
    println("----")

    val deepIndent = '''
          deeply
          indented
          content
    '''
    println("Deep indent:")
    println(deepIndent)
    println("----")

    val mixedIndent = '''
      first level
        second level
          third level
    '''
    println("Mixed indent:")
    println(mixedIndent)
    println("----")

    val withTripleQuotes = ''''
    '''
    i am cow
    '''
    ''''
    println("With triple quotes:")
    println(withTripleQuotes)
    println("----")

    val extended5 = '''''
    ''''
    content with four quotes
    ''''
    '''''
    println("Extended 5 quotes:")
    println(extended5)
    println("----")

    val normalized = '''
    line1
    line2
    '''
    println("Normalized newlines:")
    println(s"Has only LF: ${!normalized.contains('\r')}")
    println("----")

    val specialChars = '''
    !"#$%&()*+,-./:;<=>?@[\]^_`{|}~
    '''
    println("Special chars:")
    println(specialChars)
    println("----")

    val unicode = '''
    Hello 世界
    '''
    println("Unicode:")
    println(unicode)
    println("----")

    val withTabs = '''
		tab indented
		content here
	'''
    println("With tabs:")
    println(withTabs)
    println("----")

    val emptyLinesAnywhere = '''

    content

    more content

    '''
    println("Empty lines anywhere:")
    println(s"[${emptyLinesAnywhere}]")
    println("----")

    val withQuotes = '''
    "double quotes"
    'single quote'
    ''
    '''
    println("With quotes:")
    println(withQuotes)
    println("----")

    val zeroIndent = '''
    content
    '''
    println("Zero indent:")
    println(zeroIndent)
    println("----")

    val precise = '''
      ab
      cd
    '''
    println("Precise:")
    println(s"Length: ${precise.length}")
    println(s"Content: [${precise}]")
    println(s"Chars: ${precise.toList}")
    println("----")

    val name = "Alice"
    val age = 30
    val interpolated = s'''
    Hello $name
    You are $age years old
    '''
    println("Interpolated:")
    println(interpolated)
    println("----")

    val escapedInterpolated = s'''
    Hello $$name
    You are $$age years old
    '''
    println("Escaped Interpolated:")
    println(escapedInterpolated)
    println("----")

    val value = 42
    val formatted = f'''
    Value: $value%05d
    Done
    '''
    println("Formatted:")
    println(formatted)
    println("----")

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
    println("----")

    def testInterpolatedPattern(s: String): String = s match {
      case s'''
      Hello $_
      ''' => "matched greeting"
      case _ => "no match"
    }
    println("Interpolated pattern:")
    println(s"Interpolated pattern result: ${testInterpolatedPattern("Hello World")}")
    println("----")

    def testPatternTwoLines(s: String): String = s match {
      case '''
      line one
      line two
      ''' => "matched two lines"
      case _ => "no match"
    }
    println("Pattern matching (two lines):")
    println(s"Two line pattern result: ${testPatternTwoLines("line one\nline two")}")
    println("----")

    def testInterpolatedPatternTwoLines(s: String): String = s match {
      case s'''
      First: $_
      Second: $_
      ''' => "matched two line greeting"
      case _ => "no match"
    }
    println("Interpolated pattern (two lines):")
    println(s"Two line interpolated result: ${testInterpolatedPatternTwoLines("First: Alice\nSecond: Bob")}")
    println("----")

    def inFunction = '''
      function content
      more content
    '''
    println("In function:")
    println(inFunction)
    println("----")

    class InClass {
      val inClass = '''
        class member
        content
      '''
    }
    val classInstance = new InClass
    println("In class:")
    println(classInstance.inClass)
    println("----")

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
    println("----")

    val nested = "prefix" + '''
      middle
    ''' + "suffix"
    println("Nested in expressions:")
    println(nested)
    println("----")

    val typedVal: '''
      first line
        indented line
      third line
    ''' = "  first line\n    indented line\n  third line"
    println("Type ascription:")
    println(s"Value: [$typedVal]")
    println(s"Type matches: ${typedVal == "  first line\n    indented line\n  third line"}")
    println("----")

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
