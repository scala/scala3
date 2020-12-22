package dotty.dokka

import org.scalajs.dom._
import org.scalajs.dom.html.Input

class SearchbarComponent(val callback: (String) => List[PageEntry]) {
  extension (p: PageEntry)
    def toHTML = {
      val wrapper = document.createElement("div")
      wrapper.classList.add("scala3doc-searchbar-result")
      wrapper.classList.add("monospace")

      val resultA = document.createElement("a").asInstanceOf[html.Anchor]
      resultA.href = Globals.pathToRoot + p.location
      resultA.text = s"${p.name}"

      val location = document.createElement("span")
      location.classList.add("pull-right")
      location.classList.add("scala3doc-searchbar-location")
      location.textContent = p.description

      wrapper.appendChild(resultA)
      wrapper.appendChild(location)
      wrapper
    }

  def handleNewQuery(query: String) = {
    val result = callback(query).map(_.toHTML)
    while (resultsDiv.hasChildNodes()) resultsDiv.removeChild(resultsDiv.lastChild)
    result.foreach(resultsDiv.appendChild)
  }

  private val logoClick: html.Span = {
    val element = document.createElement("span").asInstanceOf[html.Span]
    element.id = "scala3doc-search"
    element.onclick = (event: Event) =>
      if (rootDiv.className.contains("hidden"))
        rootDiv.className = rootShowClasses
      else rootDiv.className = rootHiddenClasses
    document.getElementById("searchBar").appendChild(element)
    element
  }

  private val input: html.Input = {
    val element = document.createElement("input").asInstanceOf[html.Input]
    element.id = "scala3doc-searchbar-input"
    element.addEventListener("input", (e) => handleNewQuery(e.target.asInstanceOf[html.Input].value))
    element
  }

  private val resultsDiv: html.Div = {
    val element = document.createElement("div").asInstanceOf[html.Div]
    element.id = "scala3doc-searchbar-results"
    element
  }

  private val rootHiddenClasses = "hidden"
  private val rootShowClasses   = ""
  private val rootDiv: html.Div = {
    val element = document.createElement("div").asInstanceOf[html.Div]
    element.addEventListener("click", (e: Event) => e.stopPropagation())
    logoClick.addEventListener("click", (e: Event) => e.stopPropagation())
    document.body.addEventListener("click", (e: Event) => element.className = rootHiddenClasses)
    element.className = rootHiddenClasses
    element.id = "scala3doc-searchbar"
    element.appendChild(input)
    element.appendChild(resultsDiv)
    document.body.appendChild(element)
    element
  }

  handleNewQuery("")
}