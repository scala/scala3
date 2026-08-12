package dotty.tools.scaladoc

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import org.scalajs.dom.*
import org.scalajs.dom.ext.*

import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.*

import scala.scalajs.js
import scala.scalajs.js.{JSON, Promise}
import scala.scalajs.js.Thenable.Implicits.thenable2future
import utils.HTML.*

trait Versions extends js.Object:
  def versions: js.Dictionary[String]

class DropdownHandler:

  val KEY = "versions-json"
  val UNDEFINED_VERSIONS = "undefined_versions"

  private def addVersionsList(json: String) =
    val ver = JSON.parse(json).asInstanceOf[Versions]
    val ddc = document.getElementById("version-dropdown")
    val currentUrl = window.location.href
    val versionPrefix = ver.versions.keysIterator.find(currentUrl.startsWith)
    val urlSuffix = versionPrefix.map(v => currentUrl.substring(v.length)).getOrElse("")
    for (k, versionUrl) <- ver.versions do
      val child = a(cls := "text-button", href := versionUrl)(k)
      child.addEventListener("click", (e: Event) => {
        e.preventDefault()

        val fullUrl = versionUrl + urlSuffix
        val req = fetch(fullUrl, new RequestInit { method = HttpMethod.HEAD })
          .toFuture
          .map(_.status == 200)
          .recover { case _ => false }

        val timeoutPromise = scala.concurrent.Promise[Unit]()
        window.setTimeout(() => timeoutPromise.success(()), 100) // 100 ms of timeout should be plenty

        Future.firstCompletedOf(Seq(
          req,
          timeoutPromise.future.map(_ => false)
        )).foreach(linkIsAlive => window.location.href = if linkIsAlive then fullUrl else versionUrl)
      })
      ddc.appendChild(child)

  private def disableButton() =
    val btn = document.getElementById("dropdown-trigger").asInstanceOf[html.Span]
    btn.classList.add("disabled")
    btn.classList.add("hidden")

  private def getURLContent(url: String): Future[String] = fetch(url).flatMap(_.text())

  window.sessionStorage.getItem(KEY) match
    case null => // If no key, returns null
      js.typeOf(Globals.versionsDictionaryUrl) match
        case "undefined" =>
          window.sessionStorage.setItem(KEY, UNDEFINED_VERSIONS)
          disableButton()
        case _ =>
          getURLContent(Globals.versionsDictionaryUrl).onComplete {
            case Success(json: String) =>
              window.sessionStorage.setItem(KEY, json)
              addVersionsList(json)
            case Failure(_) =>
              window.sessionStorage.setItem(KEY, UNDEFINED_VERSIONS)
              disableButton()
          }
    case value => value match
      case UNDEFINED_VERSIONS =>
        disableButton()
      case json =>
        addVersionsList(json)

  document.addEventListener("click", (e: Event) => {
    document.getElementById("version-dropdown").classList.remove("expanded")
    document.getElementById("dropdown-trigger").classList.remove("selected")
  })

  document.getElementById("version-dropdown").asInstanceOf[html.Span].addEventListener("click", (e: Event) => e.stopPropagation())
end DropdownHandler

@JSExportTopLevel("dropdownHandler")
def dropdownHandler(e: Event) =
  e.stopPropagation()
  if document.getElementById("version-dropdown").getElementsByTagName("a").size > 0 &&
     window.getSelection().toString.length == 0 then
    document.getElementById("version-dropdown").classList.toggle("expanded")
    document.getElementById("dropdown-trigger").classList.toggle("selected")

@JSExportTopLevel("filterFunction")
def filterFunction() =
  val input = document.getElementById("dropdown-input").asInstanceOf[html.Input]
  val filter = input.value.toUpperCase
  val div = document.getElementById("version-dropdown")
  val as = div.getElementsByTagName("a")

  as.foreach { a =>
    val txtValue = a.innerText
    val cl = a.asInstanceOf[html.Anchor].classList
    if txtValue.toUpperCase.indexOf(filter) > -1 then
      cl.remove("filtered")
    else
      cl.add("filtered")
  }