package dotty.dokka.ux

import org.scalajs.dom._
import org.querki.jquery._
import dotty.dokka.Globals
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

def ux =
  window.addEventListener("DOMContentLoaded", _ => {
    Option(document.getElementById("leftToggler")).map {
      _.addEventListener("click", _ => document.getElementById("leftColumn").classList.toggle("open"))
    }
    val documentableElements = document.getElementsByClassName("documentableElement")
    for (i <- 0 until documentableElements.length) {
      documentableElements(i).addEventListener("click", _ => documentableElements(i).classList.toggle("expand"))
    }

    $("#sideMenu2 span").on("click", { (el: raw.Element) =>
      $(el).parent.toggleClass("expanded")
    }: js.ThisFunction0[raw.Element, Unit])

    $(".names .tab").on("click", { (el: raw.Element) => {
      val parent = $(el).parents(".tabs").first()
      val shown = $(el).hasClass("selected")
      val single = parent.hasClass("single")

      if (single) parent.find(".tab.selected").removeClass("selected")

      val id = $(el).attr("data-togglable")
      val myTab = parent.find("[data-togglable='" + id + "'].tab")

      if (!shown) { myTab.addClass("selected") }
      if (shown && !single) myTab.removeClass("selected")

      if (!shown && $(el).find(".showGraph") != null) {
        Globals.showGraph()
        $(el).find(".showGraph").removeClass("showGraph")
      }
    }}: js.ThisFunction0[raw.Element, Unit])

    Option(document.location.hash).flatMap { (x: String) =>
       Option(document.getElementById(x.substring(1)))
    }.map(_.classList.toggle("expand"))

    Option(document.getElementById("logo")).map(
      _.addEventListener("click", _ => window.location = Globals.pathToRoot) // global variable pathToRoot is created by the html renderer
    )
    Globals.hljs.registerLanguage("scala", Globals.highlightDotty)
    Globals.hljs.registerAliases(js.Array("dotty", "scala3"), "scala")
    Globals.hljs.initHighlighting()https://www.youtube.com/watch?v=NvpbW7JRu0Q&feature=youtu.be
  })
