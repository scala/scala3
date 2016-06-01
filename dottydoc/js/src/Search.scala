package dotty.tools.dottydoc
package js

import scala.scalajs.{ js => sjs }
import sjs.timers.setTimeout
import scalatags.JsDom.all._
import org.scalajs.dom.html.{ Input, Div }
import org.scalajs.dom.{ document, Event }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import js.model._
import js.model.ops._

class Search(val input: Input) {
  private var isSearching = false

  private val mainDiv = document
    .getElementById("entity-container")
    .asInstanceOf[Div]
  private val resultsDiv = document
    .getElementById("search-results")
    .asInstanceOf[Div]

  /** Search result ADTs */
  private case class PackageResults(matching: Stream[Package], nonMatching: Stream[Package])
  private object PackageResults {
    def empty = PackageResults(Stream.empty, Stream.empty)
  }

  /** Entry point into search, will consider whether to start searching or not,
   *  then call `performSearch`
   */
  def search(): Event => Unit = { e =>
    val query = input.value.trim
    if (query.length > 2) setTimeout(200) {
      if (!isSearching) {
        isSearching = true
        performSearch(query.toLowerCase).map { _ => isSearching = false }
      }
    }
    else if (query.length == 0)
      hideSearchDiv()
  }

  private def performSearch(query: String): Future[Unit] =
    for (PackageResults(matching, nonMatching) <- searchPackages(query)) yield {
      // Clear old results, add new result categories add close button
      resultsDiv.innerHTML = ""
      val toplevelRes = div(id := "toplevel-results").render
      val methodRes   = div(id := "method-results").render
      val closeButton = button(
        id := "close-button",
        onclick := { _: Event =>
          resetInput()
          hideSearchDiv()
        },
        cls := "mdl-button mdl-js-button mdl-button--fab mdl-js-ripple-effect mdl-button--colored",
        i(cls := "material-icons", "clear")
      ).render

      resultsDiv.appendChild(toplevelRes)
      resultsDiv.appendChild(methodRes)
      resultsDiv.appendChild(closeButton)

      // Add all matching toplevel entities
      matching
        .toStream
        .sortBy(_.name)
        .map(createPackageCard)
        .foreach(toplevelRes.appendChild)

      // Hide entity and show results instead
      showSearchDiv()
    }

  private def searchPackages(query: String): Future[PackageResults] = Future {
    EntityIndex.packages.values.foldLeft(PackageResults.empty) { (acc, p) =>
      val matchingMembers = p.members.collect {
        case x if (x.name.toLowerCase.contains(query) && x.kind != "package") => x
      }

      println("We found all matching members maybe")
      sjs.Dynamic.global.console.log(p)

      if (p.name.toLowerCase.contains(query) || matchingMembers.nonEmpty)
        acc.copy(matching = p.withMembers(matchingMembers) #:: acc.matching)
      else acc
    }
  }

  private def createPackageCard(pack: Package): Div = div(
    cls := "mdl-card mdl-shadow--2dp package-result",
    div(cls := "mdl-card__title", h2(cls := "mdl-card__title-text", pack.name)),
    ul(pack.members.map(createEntityLi).toList)
  ).render

  private def createEntityLi(e: Entity) = li(
    a(
      href := toRoot + e.path.mkString("", "/", ".html"),
      e.kind + " " + e.name
    )
  )

  private def toRoot = "../" * (EntityIndex.currentEntity.path.length - 1)

  private def resetInput() = {
    input.value = ""
    input.parentElement.className = input.parentElement.className.replaceAll("is-dirty", "")
  }

  private def showSearchDiv() = {
    mainDiv.style.display    = "none"
    resultsDiv.style.display = "block"
  }

  private def hideSearchDiv() = {
    resultsDiv.style.display = "none"
    mainDiv.style.display = "block"
  }
}
