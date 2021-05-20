package dotty.tools.scaladoc

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success,Failure}

import org.scalajs.dom._
import org.scalajs.dom.ext._
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js
import scala.scalajs.js.JSON

trait Versions extends js.Object:
  def versions: js.Dictionary[String]

class DropdownHandler:

  val KEY = "versions-json"
  val UNDEFINED_VERSIONS = "undefined_versions"

  private def addVersionsList(json: String) =
    val ver = JSON.parse(json).asInstanceOf[Versions]
    val ddc = document.getElementById("dropdown-content")
    for (k, v) <- ver.versions do
      var child = document.createElement("a").asInstanceOf[html.Anchor]
      child.href = v
      child.text = k
      ddc.appendChild(child)

  private def disableButton() =
    val btn = document.getElementById("dropdown-button").asInstanceOf[html.Button]
    btn.disabled = true
    btn.classList.remove("dropdownbtnactive")

  private def getURLContent(url: String): Future[String] = Ajax.get(url).map(_.responseText)

  window.localStorage.getItem(KEY) match
    case null => // If no key, returns null
      Globals.versionsDictionaryUrl match
        case null => // global property not defined
          // do nothing
        case url: String =>
          getURLContent(url).onComplete {
            case Success(json: String) =>
              window.localStorage.setItem(KEY, json)
              addVersionsList(json)
            case Failure(_) =>
              window.localStorage.setItem(KEY, UNDEFINED_VERSIONS)
              disableButton()
          }
    case value => value match
      case UNDEFINED_VERSIONS =>
        disableButton()
      case json =>
        addVersionsList(json)


  document.onclick = (e: Event) => {
    if e.target.asInstanceOf[html.Element].id != "dropdown-button" then
      document.getElementById("dropdown-content").classList.remove("show")
  }

  document.getElementById("version").asInstanceOf[html.Span].onclick = (e: Event) => {
    e.stopPropagation
  }


@JSExportTopLevel("dropdownHandler")
def dropdownHandler() =
  if document.getElementById("dropdown-content").getElementsByTagName("a").size > 0 &&
     window.getSelection.toString.length == 0 then
    document.getElementById("dropdown-content").classList.toggle("show")

@JSExportTopLevel("filterFunction")
def filterFunction() =
  val input = document.getElementById("dropdown-input").asInstanceOf[html.Input]
  val filter = input.value.toUpperCase
  val div = document.getElementById("dropdown-content")
  val a = div.getElementsByTagName("a")
  for i <- 0 until a.length do
    val txtValue = a(i).innerText
    val disp = if txtValue.toUpperCase.indexOf(filter) > -1 then
      ""
    else
      "none"
    a(i).asInstanceOf[html.Anchor].style.display = disp
