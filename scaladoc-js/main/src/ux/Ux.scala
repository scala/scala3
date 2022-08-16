package dotty.tools.scaladoc

import scala.scalajs.js
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

  def loadConciseView(): Unit =
    val localStorageValue = SafeLocalStorage("__CONCISE_VIEW__", js.Array(false)) // One-element js.Array is a hack for having type extending js.Any
    val conciseViewSwitchInput = Option(document.getElementById("concise-view-switch"))
      .map(_.querySelector("input").asInstanceOf[html.Input])

    def modifyContent(concise: Boolean) =
      if (concise) {
        document.querySelector(".membersList").classList.add("concise")
      } else {
        document.querySelector(".membersList").classList.remove("concise")
      }

    conciseViewSwitchInput.foreach { input =>
      val storedValue = localStorageValue.getData.head
      modifyContent(storedValue)
      input.checked = storedValue
      input.addEventListener("change", e => {
        val target = e.target.asInstanceOf[html.Input]
        localStorageValue.setData(js.Array(target.checked))
        modifyContent(target.checked)
      })
    }

  sideMenuItemsWordBreaking()
  loadConciseView()



