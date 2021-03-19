package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

class CodeSnippets:
  val replacePatternsAndResults: Seq[(String, String)] = Seq(
    """(\/\/{{\n)((.|\n)*?)(\/\/}}\n)""" -> """<span class="hideable">$2</span>""", // wrap content of block directives
    """(\/\/.*?)(\n|\$)""" -> """<span class="hideable">$1</span>$2""", // wrap single line comment
    """(\/\*)((.|\n)*?)(\*\/)""" -> """<span class="hideable">$0</span>""", // wrap multi line comment
  )

  document.querySelectorAll("code").foreach {
    case e: html.Element => e.innerHTML = replacePatternsAndResults.foldLeft(e.innerHTML) {
      case (acc, (pattern, result)) =>
        acc.replaceAll(pattern, result)
    }
  }

  def toggleHide(e: html.Element | html.Document) = e.querySelectorAll("code span.hideable").foreach {
    case e: html.Element if e.style.getPropertyValue("display").isEmpty => e.style.setProperty("display", "none")
    case e: html.Element => e.style.removeProperty("display")
  }

  toggleHide(document)

  document.querySelectorAll("pre").foreach {
    case e: html.Element =>
      val a = document.createElement("a")
      a.textContent = "Show"
      a.addEventListener("click", { (_: MouseEvent) =>
        a.textContent = if a.textContent == "Show" then "Hide" else "Show"
        toggleHide(e)
      })
      a.classList.add("hide-snippet-comments-button")
      e.insertBefore(a, e.firstChild)
  }
