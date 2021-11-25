package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

import scala.util.matching.Regex._
import scala.util.matching._

class Ux():
  def sideMenuItemsWordBreaking(): Unit =
    val matchingRegex = raw"([.A-Z])".r

    def modifySpan = (span: html.Span) => {
      val textNodes = span.childNodes.filter(_.nodeType == 3)
      val texts = textNodes.map(_.nodeValue).mkString
      span.innerHTML = matchingRegex.replaceAllIn(texts, m => s"<wbr>${m.group(0)}")
    }

    val nodes = document.querySelectorAll("#sideMenu2 a span").collect {
        case e: html.Span => e
      }.foreach(modifySpan)

  sideMenuItemsWordBreaking()