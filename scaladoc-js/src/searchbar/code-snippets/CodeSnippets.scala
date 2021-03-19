package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

class CodeSnippets:
  val replacePatternsAndResults = Seq(
    // ("", ""), // hide block directives
    ("""(\/\/.*)(\n|\$)""", ""), // single line comment
    // ("", ""), // multi line comment
  )

  document.querySelectorAll("code").foreach {
    case e: html.Element => e.innerHTML = replacePatternsAndResults.foldLeft(e.innerHTML) {
      case (acc, (pattern, result)) => acc.replaceAll(pattern, """<span class="hideable;">$1</span>""")
    }
  }
