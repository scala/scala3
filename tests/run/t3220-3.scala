object Literals {
  //unicode escapes don't get expanded in comments
  def comment = "comment" //\u000A is the bomb
  //unicode escapes work in string
  def inString = "\u000A"
  def inTripleQuoted = """\u000A"""
  def inRawInterpolation = raw"\u000A"
  def inRawTripleQuoted = raw"""\u000A"""
  def inChar = '\u000A'
  def `in backtick quoted\u0020identifier` = "bueno"
  //unicode escapes preceded by an odd number of backslash characters
  //are not processed iff the backslashes themselves are escaped
  def after2slashestriple = """\\u0040"""
  def after2slashesplain = "\\u0040"
  def after2slashesraw = raw"\\u0040"
  def after2slashess = s"\\u0040"
  def firstFailure = ("\""+"""([^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"")
  def badString = """bad\"""
  def escapedQuotesInInterpolation = s"\u0022_\u0022"
  def escapedQuotesInSingleQuotedString = "\u0022"
  def escapedQuotesInCharLit = '\u0027'


  def processed = List(
    "literal tab in single quoted string" -> "tab	tab",
    "tab escape char in single quoted string" -> "tab\ttab",
    "tab unicode escape in single quoted string" -> "tab\u0009tab",
    "literal tab in triple quoted string" -> """tab	tab""",
    "literal tab in triple quoted raw interpolator" -> raw"""tab	tab""",
    "literal tab in single quoted raw interpolator" -> raw"tab	tab",
    "literal tab in triple quoted s interpolator" -> s"""tab	tab""",
    "literal tab in single quoted s interpolator" -> s"tab	tab",
    "tab escape char in triple quoted s interpolator" -> s"""tab\ttab""",
    "tab escape char in single quoted s interpolator" -> s"""tab\ttab""",
    "tab unicode escape in triple quoted s interpolator" -> s"""tab\u0009tab""",
    "tab unicode escape in single quoted s interpolator" -> s"tab\u0009tab"
  )
}

object Test {
  def main(args: Array[String]): Unit = {
    val bueono = Literals.`in backtick quoted identifier`

    def printways(ways: List[(String, String)]) =
      ways.map(_._1).sorted.mkString(", ")

    def printSegment(l: List[(String, String)]) =
      l.groupBy(_._2).toList.foreach{
       case (result, ways) => {
         println(s"literals that result in $result:")
         ways.foreach{case (x, _) => println(x)}
         println()
       }
     }

    print("processed...")

    for {
      case (description, format) <- Literals.processed
    } {
      assert(format == "tab\ttab", description)
    }
    println("OK")

    print("unprocessed...")
    assert("""t\tt""".toList == List('t', '\\', 't', 't'), "tab escape char in triple quoted string")
    assert("""tab\ttab""" == raw"tab\ttab", "tab escape char in raw interpolator")
    assert("""tab\ttab""" == raw"""tab\ttab""", "tab escape char in raw triple quoted interpolator")
    println("OK")

    println("after backslashes")
    println(Literals.after2slashestriple.toList)
    println(Literals.after2slashesplain.toList)
    println(Literals.after2slashesraw.toList)
    println(Literals.after2slashess.toList)
    println(Literals.firstFailure.toList)
    println(Literals.badString.toList)

    val asList = List('\\', 'u', '0', '0', '0', 'A')
    assert(asList == Literals.inTripleQuoted.toList)
    assert(asList == Literals.inRawInterpolation.toList)
    assert(asList == Literals.inRawTripleQuoted.toList)

  }
}