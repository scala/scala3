package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.embedded._

object Code {

  // Default positions
  val m1 = new CodeMarker("m1")
  val m2 = new CodeMarker("m2")
  val m3 = new CodeMarker("m3")
  val m4 = new CodeMarker("m4")
  val m5 = new CodeMarker("m5")
  val m6 = new CodeMarker("m6")
  val m7 = new CodeMarker("m7")
  val m8 = new CodeMarker("m8")

  implicit class CodeHelper(val sc: StringContext) extends AnyVal {
    def code(args: Embedded*): SourceWithPositions = {
      val pi = sc.parts.iterator
      val ai = args.iterator

      var line = 0
      var char = 0
      def scan(str: String): Unit = {
        for (c <- str)
          if (c == '\n') { line += 1; char = 0 } else { char += 1 }
      }

      val stringBuilder = new StringBuilder
      val positions = List.newBuilder[(CodeMarker, Int, Int)]

      while (ai.hasNext) {
        val next = pi.next().stripMargin
        stringBuilder.append(next)
        scan(next)

        ai.next() match {
          case emb: CodeMarker =>
            positions += Tuple3(emb, line, char)

          case emb: CodeInRange =>
            positions += Tuple3(emb.range.start, line, char)
            scan(emb.text)
            stringBuilder.append(emb.text)
            positions += Tuple3(emb.range.end, line, char)
        }

      }

      if (pi.hasNext)
        stringBuilder.append(pi.next())

      SourceWithPositions(stringBuilder.result(), positions.result())
    }
  }

  def withSources(sources: SourceWithPositions*): CodeTester = new CodeTester(sources.toList, Nil)

  case class SourceWithPositions(text: String, positions: List[(CodeMarker, Int, Int)]) {
    def withSource: CodeTester =  new CodeTester(this :: Nil, Nil)
  }

}
