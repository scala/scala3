package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

class CodeSnippets:
  def toggleHide(e: html.Element | html.Document) = e.querySelectorAll("code span.hideable").foreach {
    case e: html.Element if e.style.getPropertyValue("display").isEmpty => e.style.setProperty("display", "none")
    case e: html.Element => e.style.removeProperty("display")
  }

  toggleHide(document)

  document.querySelectorAll("pre").foreach {
    case e: html.Element if e.querySelectorAll("code span.hideable").nonEmpty =>
      val a = document.createElement("a")
      a.addEventListener("click", { (_: MouseEvent) =>
        if(a.classList.contains("hide-snippet-comments-button")) {
          a.classList.remove("hide-snippet-comments-button")
        } else {
          a.classList.add("hide-snippet-comments-button")
        }
        toggleHide(e)
      })
      a.classList.add("snippet-comment-button")
      e.insertBefore(a, e.firstChild)
    case e => // skip
  }
