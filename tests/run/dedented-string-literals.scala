import language.experimental.dedentedStringLiterals
import scala.compiletime.constValue

@scala.annotation.compileTimeOnly('''
  compile-time only message
  ''')
def compileTimeOnlyDedentedMessage: Unit = ()

@scala.annotation.implicitNotFound('''
  missing dedented evidence
  ''')
trait DedentedImplicitNotFound

object Test {
  def check(label: String, actual: String, expected: String): Unit =
    if actual != expected then
      throw AssertionError(s"$label\nactual:   [$actual]\nexpected: [$expected]")

  def main(args: Array[String]): Unit = {
    check("basic", '''
    i am cow
    hear me moo
    ''', "i am cow\nhear me moo")

    check("noIndent", '''
i am cow
hear me moo
''', "i am cow\nhear me moo")

    check("withIndentPreserved", '''
      i am cow
      hear me moo
    ''', "  i am cow\n  hear me moo")

    check("empty", '''
    ''', "")

    check("singleLine", '''
    hello world
    ''', "hello world")

    check("blankLines", '''
    line 1

    line 3
    ''', "line 1\n\nline 3")

    check("deepIndent", '''
          deeply
          indented
          content
    ''', "      deeply\n      indented\n      content")

    check("mixedIndent", '''
      first level
        second level
          third level
    ''', "  first level\n    second level\n      third level")

    check("withTripleQuotes", ''''
    '''
    i am cow
    '''
    '''', "'''\ni am cow\n'''")

    check("extended5", '''''
    ''''
    content with four quotes
    ''''
    ''''', "''''\ncontent with four quotes\n''''")

    val normalized = '''
    line1
    line2
    '''
    assert(!normalized.contains('\r'), "dedented strings should normalize line endings to LF")
    check("normalized", normalized, "line1\nline2")

    check("specialChars", '''
    !"#$%&()*+,-./:;<=>?@[\]^_`{|}~
    ''', """!"#$%&()*+,-./:;<=>?@[\]^_`{|}~""")

    check("unicode", '''
    Hello 世界
    ''', "Hello 世界")

    check("withTabs", '''
		tab indented
		content here
	''', "\ttab indented\n\tcontent here")

    check("emptyLinesAnywhere", '''

    content

    more content

    ''', "\ncontent\n\nmore content\n")

    check("withQuotes", '''
    "double quotes"
    'single quote'
    ''
    ''', "\"double quotes\"\n'single quote'\n''")

    val precise = '''
      ab
      cd
    '''
    check("precise", precise, "  ab\n  cd")
    assert(precise.length == 9)

    val name = "Alice"
    val age = 30

    check("interpolated", s'''
    Hello $name
    You are $age years old
    ''', "Hello Alice\nYou are 30 years old")

    check("escapedInterpolated", s'''
    Hello $$name
    You are $$age years old
    ''', "Hello $name\nYou are $age years old")

    check("quoteEscapeInterpolated", s'''
    single quote: $'
    dollar: $$
    ''', "single quote: '\ndollar: $")

    check("multipleInterpolations", s'''
    a $name b ${age + 1} c
    ''', "a Alice b 31 c")

    check("formatted", f'''
    Value: $age%05d
    Done
    ''', "Value: 00030\nDone")

    check("contentsWithNewline", s'''
    ${"content with\nnewline"}
    more text
    ''', "content with\nnewline\nmore text")

    check("bracedInterpolationThenNewline", s'''
    first ${1 + 1}
    second
    ''', "first 2\nsecond")

    check("nestedInterpolator", s'''
    outer ${'''
      inner
      '''}
    ''', "outer inner")

    check("nestedInterpolator2", s'''
    outer ${s'''
      inner with ${1+1}
      '''}
    ''', "outer inner with 2")

    check("extendedInterpolated", s''''
    ''' $name
    '''', "''' Alice")

    def testPattern(s: String): String = s match {
      case '''
      test
      ''' => "matched basic"
      case '''
      other
      ''' => "matched other"
      case _ => "no match"
    }
    check("pattern", testPattern("test"), "matched basic")

    def testInterpolatedPattern(s: String): String = s match {
      case s'''
      Hello $_
      ''' => "matched greeting"
      case _ => "no match"
    }
    check("interpolatedPattern", testInterpolatedPattern("Hello World"), "matched greeting")

    def testPatternTwoLines(s: String): String = s match {
      case '''
      line one
      line two
      ''' => "matched two lines"
      case _ => "no match"
    }
    check("patternTwoLines", testPatternTwoLines("line one\nline two"), "matched two lines")

    def testInterpolatedPatternTwoLines(s: String): String = s match {
      case s'''
      First: $_
      Second: $_
      ''' => "matched two line greeting"
      case _ => "no match"
    }
    check("interpolatedPatternTwoLines", testInterpolatedPatternTwoLines("First: Alice\nSecond: Bob"), "matched two line greeting")

    def inFunction = '''
      function content
      more content
    '''
    check("inFunction", inFunction, "  function content\n  more content")

    class InClass {
      val inClass = '''
        class member
        content
      '''
    }
    check("inClass", new InClass().inClass, "  class member\n  content")

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
    assert(list == List("  first", "  second", "  third"))

    val nested = "prefix" + '''
      middle
    ''' + "suffix"
    check("nestedExpression", nested, "prefix  middlesuffix")

    val typedVal: '''
      first line
        indented line
      third line
    ''' = "  first line\n    indented line\n  third line"
    check("typeAscription", typedVal, "  first line\n    indented line\n  third line")

    val valueOfResult = constValue['''
      alpha
        beta
      gamma
    ''']
    check("constValue", valueOfResult, "  alpha\n    beta\n  gamma")

    println("OK")
  }
}