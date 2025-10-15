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
  }
}
