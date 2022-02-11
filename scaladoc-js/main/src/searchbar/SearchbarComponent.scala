package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.html.Input
import scala.scalajs.js.timers._
import scala.concurrent.duration._

class SearchbarComponent(engine: SearchbarEngine, inkuireEngine: InkuireJSSearchEngine, parser: QueryParser):
  val resultsChunkSize = 100
  extension (p: PageEntry)
    def toHTML =
      val wrapper = document.createElement("div").asInstanceOf[html.Div]
      wrapper.classList.add("scaladoc-searchbar-result")
      wrapper.classList.add("scaladoc-searchbar-result-row")
      wrapper.classList.add("monospace")

      val icon = document.createElement("span").asInstanceOf[html.Span]
      icon.classList.add("micon")
      icon.classList.add(p.kind.take(2))

      val resultA = document.createElement("a").asInstanceOf[html.Anchor]
      resultA.href = Globals.pathToRoot + p.location
      resultA.text = s"${p.fullName}"
      resultA.onclick = (event: Event) =>
        if (document.body.contains(rootDiv)) {
          document.body.removeChild(rootDiv)
        }

      val location = document.createElement("span")
      location.classList.add("pull-right")
      location.classList.add("scaladoc-searchbar-location")
      location.textContent = p.description

      wrapper.appendChild(icon)
      wrapper.appendChild(resultA)
      resultA.appendChild(location)
      wrapper.addEventListener("mouseover", {
        case e: MouseEvent => handleHover(wrapper)
      })
      wrapper

  extension (m: InkuireMatch)
    def toHTML =
      val wrapper = document.createElement("div").asInstanceOf[html.Div]
      wrapper.classList.add("scaladoc-searchbar-result")
      wrapper.classList.add("monospace")
      wrapper.setAttribute("mq", m.mq.toString)

      val resultDiv = document.createElement("div").asInstanceOf[html.Div]
      resultDiv.classList.add("scaladoc-searchbar-result-row")

      val icon = document.createElement("span").asInstanceOf[html.Span]
      icon.classList.add("micon")
      icon.classList.add(m.entryType.take(2))

      val resultA = document.createElement("a").asInstanceOf[html.Anchor]
      resultA.href = m.pageLocation
      resultA.text = m.functionName
      resultA.onclick = (event: Event) =>
        if (document.body.contains(rootDiv)) {
          document.body.removeChild(rootDiv)
        }

      val packageDiv = document.createElement("div").asInstanceOf[html.Div]
      packageDiv.classList.add("scaladoc-searchbar-inkuire-package")

      val packageIcon = document.createElement("span").asInstanceOf[html.Span]
      packageIcon.classList.add("micon")
      packageIcon.classList.add("pa")

      val packageSpan = document.createElement("span").asInstanceOf[html.Span]
      packageSpan.textContent = m.packageLocation

      val signature = document.createElement("span")
      signature.classList.add("pull-right")
      signature.classList.add("scaladoc-searchbar-inkuire-signature")
      signature.textContent = m.prettifiedSignature

      wrapper.appendChild(resultDiv)
      resultDiv.appendChild(icon)
      resultDiv.appendChild(resultA)
      resultA.appendChild(signature)
      wrapper.appendChild(packageDiv)
      packageDiv.appendChild(packageIcon)
      packageDiv.appendChild(packageSpan)
      wrapper.addEventListener("mouseover", {
        case e: MouseEvent => handleHover(wrapper)
      })
      wrapper

  def handleNewFluffQuery(matchers: List[Matchers]) =
    val result = engine.query(matchers).map(_.toHTML)
    resultsDiv.scrollTop = 0
    while (resultsDiv.hasChildNodes()) resultsDiv.removeChild(resultsDiv.lastChild)
    val fragment = document.createDocumentFragment()
    result.take(resultsChunkSize).foreach(fragment.appendChild)
    resultsDiv.appendChild(fragment)
    def loadMoreResults(result: List[raw.HTMLElement]): Unit = {
      resultsDiv.onscroll = (event: Event) => {
        if (resultsDiv.scrollHeight - resultsDiv.scrollTop == resultsDiv.clientHeight) {
          val fragment = document.createDocumentFragment()
          result.take(resultsChunkSize).foreach(fragment.appendChild)
          resultsDiv.appendChild(fragment)
          loadMoreResults(result.drop(resultsChunkSize))
        }
      }
    }
    loadMoreResults(result.drop(resultsChunkSize))

  extension (s: String)
    def toHTMLError =
      val wrapper = document.createElement("div").asInstanceOf[html.Div]
      wrapper.classList.add("scaladoc-searchbar-result")
      wrapper.classList.add("monospace")

      val errorSpan = document.createElement("span").asInstanceOf[html.Span]
      errorSpan.classList.add("search-error")
      errorSpan.textContent = s

      wrapper.appendChild(errorSpan)
      wrapper

  var timeoutHandle: SetTimeoutHandle = null
  def handleNewQuery(query: String) =
    clearTimeout(timeoutHandle)
    resultsDiv.scrollTop = 0
    resultsDiv.onscroll = (event: Event) => { }
    while (resultsDiv.hasChildNodes()) resultsDiv.removeChild(resultsDiv.lastChild)
    val fragment = document.createDocumentFragment()
    parser.parse(query) match {
      case EngineMatchersQuery(matchers) =>
        handleNewFluffQuery(matchers)
      case BySignature(signature) =>
        timeoutHandle = setTimeout(1.second) {
          val properResultsDiv = document.createElement("div").asInstanceOf[html.Div]
          resultsDiv.appendChild(properResultsDiv)
          val loading = document.createElement("div").asInstanceOf[html.Div]
          loading.classList.add("loading-wrapper")
          val animation = document.createElement("div").asInstanceOf[html.Div]
          animation.classList.add("loading")
          loading.appendChild(animation)
          properResultsDiv.appendChild(loading)
          inkuireEngine.query(query) { (m: InkuireMatch) =>
            val next = properResultsDiv.children.foldLeft[Option[Element]](None) {
              case (acc, child) if !acc.isEmpty => acc
              case (_, child) =>
                Option.when(child.hasAttribute("mq") && Integer.parseInt(child.getAttribute("mq")) > m.mq)(child)
            }
            next.fold {
              properResultsDiv.appendChild(m.toHTML)
            } { next =>
              properResultsDiv.insertBefore(m.toHTML, next)
            }
          } { (s: String) =>
            animation.classList.remove("loading")
            properResultsDiv.appendChild(s.toHTMLError)
          }
        }
    }

  private val searchIcon: html.Div =
    val span = document.createElement("span").asInstanceOf[html.Span]
    span.innerHTML = """<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20"><path d="M19.64 18.36l-6.24-6.24a7.52 7.52 0 10-1.28 1.28l6.24 6.24zM7.5 13.4a5.9 5.9 0 115.9-5.9 5.91 5.91 0 01-5.9 5.9z"></path></svg>"""
    span.id = "scaladoc-search"
    span.onclick = (event: Event) =>
      if (document.body.contains(rootDiv)) {
        document.body.removeChild(rootDiv)
      }
      else {
        document.body.appendChild(rootDiv)
        input.focus()
      }
    // open the search if the user hits the `s` key when not focused on a text input
    document.body.addEventListener("keydown", (e: KeyboardEvent) => handleGlobalKeyDown(e))

    val element = createNestingDiv("search-content")(
      createNestingDiv("search-container")(
        createNestingDiv("search")(
          span
        )
      )
    )
    document.getElementById("scaladoc-searchBar").appendChild(element)
    element

  private val input: html.Input =
    val element = document.createElement("input").asInstanceOf[html.Input]
    element.id = "scaladoc-searchbar-input"
    element.addEventListener("input", (e) => handleNewQuery(e.target.asInstanceOf[html.Input].value))
    element.autocomplete = "off"
    element

  private val resultsDiv: html.Div =
    val element = document.createElement("div").asInstanceOf[html.Div]
    element.id = "scaladoc-searchbar-results"
    element

  private val rootHiddenClasses = "hidden"
  private val rootShowClasses   = ""

  private def createNestingDiv(className: String)(innerElement: html.Element): html.Div =
    val element = document.createElement("div").asInstanceOf[html.Div]
    element.className = className
    element.appendChild(innerElement)
    element

  private val rootDiv: html.Div =
    val element = document.createElement("div").asInstanceOf[html.Div]
    element.addEventListener("mousedown", (e: Event) => e.stopPropagation())
    searchIcon.addEventListener("mousedown", (e: Event) => e.stopPropagation())
    document.body.addEventListener("mousedown", (e: Event) =>
      if (document.body.contains(element)) {
        document.body.removeChild(element)
      }
    )
    element.addEventListener("keydown", {
      case e: KeyboardEvent =>
        if e.keyCode == 40 then handleArrowDown()
        else if e.keyCode == 38 then handleArrowUp()
        else if e.keyCode == 13 then handleEnter()
        else if e.keyCode == 27 then handleEscape()
    })
    element.id = "scaladoc-searchbar"
    element.appendChild(input)
    element.appendChild(resultsDiv)
    element

  private def handleArrowUp() = {
    val selectedElement = resultsDiv.querySelector("[selected]")
    if selectedElement != null then {
      selectedElement.removeAttribute("selected")
      val sibling = selectedElement.previousElementSibling
      if sibling != null && sibling.classList.contains("scaladoc-searchbar-result") then {
        sibling.setAttribute("selected", "")
        resultsDiv.scrollTop = sibling.asInstanceOf[html.Element].offsetTop - (2 * sibling.asInstanceOf[html.Element].clientHeight)
      }
    }
  }
  private def handleArrowDown() = {
    val selectedElement = resultsDiv.querySelector("[selected]")
    if selectedElement != null then {
      val sibling = selectedElement.nextElementSibling
      if sibling != null then {
        selectedElement.removeAttribute("selected")
        sibling.setAttribute("selected", "")
        resultsDiv.scrollTop = sibling.asInstanceOf[html.Element].offsetTop - (2 * sibling.asInstanceOf[html.Element].clientHeight)
      }
    } else {
      val firstResult = resultsDiv.firstElementChild
      if firstResult != null && firstResult.classList.contains("scaladoc-searchbar-result") then {
        firstResult.setAttribute("selected", "")
        resultsDiv.scrollTop = firstResult.asInstanceOf[html.Element].offsetTop - (2 * firstResult.asInstanceOf[html.Element].clientHeight)
      } else if firstResult != null && firstResult.firstElementChild != null && firstResult.firstElementChild.nextElementSibling != null then {
        // for Inkuire there is another wrapper to avoid displaying old results + the first (child) div is a loading animation wrapper | should be resolved in #12995
        val properFirstResult = firstResult.firstElementChild.nextElementSibling
        properFirstResult.setAttribute("selected", "")
        resultsDiv.scrollTop = properFirstResult.asInstanceOf[html.Element].offsetTop - (2 * properFirstResult.asInstanceOf[html.Element].clientHeight)
      }
    }
  }
  private def handleEnter() = {
    val selectedElement = resultsDiv.querySelector("[selected] a").asInstanceOf[html.Element]
    if selectedElement != null then {
      selectedElement.click()
    }
  }
  private def handleEscape() = {
    // clear the search input and close the search
    input.value = ""
    handleNewQuery("")
    document.body.removeChild(rootDiv)
  }

  private def handleHover(elem: html.Element) = {
    val selectedElement = resultsDiv.querySelector("[selected]")
    if selectedElement != null then {
      selectedElement.removeAttribute("selected")
    }
    elem.setAttribute("selected","")
  }

  private def handleGlobalKeyDown(e: KeyboardEvent) = {
    // if the user presses the "S" key while not focused on an input, open the search
    if (e.key == "s" || e.key == "/") {
      val tag = e.target.asInstanceOf[html.Element].tagName
      if (tag != "INPUT" && tag != "TEXTAREA") {
        if (!document.body.contains(rootDiv)) {
          // Firefox's "quick find" uses "/" as a trigger; prevent that.
          e.preventDefault()

          document.body.appendChild(rootDiv)
          // if we focus during the event handler, the `s` gets typed into the input
          window.setTimeout(() => input.focus(), 1.0)
        }
      }
    }
  }

  handleNewQuery("")
