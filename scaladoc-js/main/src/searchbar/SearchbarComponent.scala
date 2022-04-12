package dotty.tools.scaladoc

import utils.HTML._

import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.html.Input
import scala.scalajs.js.timers._
import scala.concurrent.duration.{span => dspan, _}

import scala.util.chaining._

import java.net.URI

class SearchbarComponent(engine: SearchbarEngine, inkuireEngine: InkuireJSSearchEngine, parser: QueryParser):
  val initialChunkSize = 5
  val resultsChunkSize = 20
  extension (p: PageEntry)
    def toHTML =
      val location = if (p.isLocationExternal) {
        p.location
      } else {
        Globals.pathToRoot + p.location
      }

      div(cls := "scaladoc-searchbar-row monospace", "result" := "")(
        a(href := location)(
          p.fullName,
          span(cls := "pull-right scaladoc-searchbar-location")(p.description)
        ).tap { _.onclick = (event: Event) =>
          if (document.body.contains(rootDiv)) {
            document.body.removeChild(rootDiv)
          }
        }
      ).tap { wrapper => wrapper.addEventListener("mouseover", {
          case e: MouseEvent => handleHover(wrapper)
        })
      }

  extension (m: InkuireMatch)
    def toHTML =
      val location = if (m.pageLocation(0) == 'e') {
          m.pageLocation.substring(1)
        } else {
          Globals.pathToRoot + m.pageLocation.substring(1)
        }

      div(cls := "scaladoc-searchbar-row monospace", "result" := "", "inkuire-result" := "", "mq" := m.mq.toString)(
        a(href := location)(
          m.functionName,
          span(cls := "pull-right scaladoc-searchbar-inkuire-signature")(m.prettifiedSignature)
        ).tap { _.onclick = (event: Event) =>
          if (document.body.contains(rootDiv)) {
            document.body.removeChild(rootDiv)
          }
        },
        div(cls := "scaladoc-searchbar-inkuire-package")(
          span(cls := "micon pa"),
          span(m.packageLocation)
        )
      ).tap { wrapper => wrapper.addEventListener("mouseover", {
          case e: MouseEvent => handleHover(wrapper)
        })
      }

  def createKindSeparator(kind: String) =
    div(cls := "scaladoc-searchbar-row monospace", "divider" := "")(
      span(cls := s"micon ${kind.take(2)}"),
      span(kind)
    )

  def handleNewFluffQuery(matchers: List[Matchers]) =
    val result = engine.query(matchers)
    val fragment = document.createDocumentFragment()
    def createLoadMoreElement =
      div(cls := "scaladoc-searchbar-row monospace", "loadmore" := "")(
        a(
          a(cls := "i fas fa-arrow-down"),
          span("Show more...")
        )
      ).tap { loadMoreElement => loadMoreElement
        .addEventListener("mouseover", _ => handleHover(loadMoreElement))
      }

    result.groupBy(_.kind).map {
      case (kind, entries) =>
        val kindSeparator = createKindSeparator(kind)
        val htmlEntries = entries.map(_.toHTML)
        val loadMoreElement = createLoadMoreElement
        def loadMoreResults(entries: List[raw.HTMLElement]): Unit = {
          loadMoreElement.onclick = (event: Event) => {
            entries.take(resultsChunkSize).foreach(_.classList.remove("hidden"))
            val nextElems = entries.drop(resultsChunkSize)
            if nextElems.nonEmpty then loadMoreResults(nextElems) else loadMoreElement.classList.add("hidden")
          }
        }

        fragment.appendChild(kindSeparator)
        htmlEntries.foreach(fragment.appendChild)
        fragment.appendChild(loadMoreElement)

        val nextElems = htmlEntries.drop(initialChunkSize)
        if nextElems.nonEmpty then {
          nextElems.foreach(_.classList.add("hidden"))
          loadMoreResults(nextElems)
        } else {
          loadMoreElement.classList.add("hidden")
        }

    }

    resultsDiv.scrollTop = 0
    while (resultsDiv.hasChildNodes()) resultsDiv.removeChild(resultsDiv.lastChild)
    resultsDiv.appendChild(fragment)

  def createLoadingAnimation: raw.HTMLElement =
    div(cls := "loading-wrapper")(
      div(cls := "loading")
    )
  extension (s: String)
    def toHTMLError =
      div(cls := "scaladoc-searchbar-row monospace", "error" := "")(
        span(cls := "search-error")(s)
      )

  var timeoutHandle: SetTimeoutHandle = null
  def handleNewQuery(query: String) =
    clearTimeout(timeoutHandle)
    resultsDiv.scrollTop = 0
    resultsDiv.onscroll = (event: Event) => { }
    def clearResults() = while (resultsDiv.hasChildNodes()) resultsDiv.removeChild(resultsDiv.lastChild)
    val fragment = document.createDocumentFragment()
    parser.parse(query) match {
      case EngineMatchersQuery(matchers) =>
        clearResults()
        handleNewFluffQuery(matchers)
      case BySignature(signature) =>
        timeoutHandle = setTimeout(1.second) {
          val loading = createLoadingAnimation
          val kindSeparator = createKindSeparator("inkuire")
          clearResults()
          resultsDiv.appendChild(loading)
          resultsDiv.appendChild(kindSeparator)
          inkuireEngine.query(query) { (m: InkuireMatch) =>
            val next = resultsDiv.children
              .find(child => child.hasAttribute("mq") && Integer.parseInt(child.getAttribute("mq")) > m.mq)
            next.fold {
              resultsDiv.appendChild(m.toHTML)
            } { next =>
              resultsDiv.insertBefore(m.toHTML, next)
            }
          } { (s: String) =>
            resultsDiv.removeChild(loading)
            resultsDiv.appendChild(s.toHTMLError)
          }
        }
    }

  private val searchIcon: html.Button =
    val icon = document.getElementById("search-toggle").asInstanceOf[html.Button]
    icon.onclick = (event: Event) =>
      if (document.body.contains(rootDiv)) {
        document.body.removeChild(rootDiv)
      }
      else {
        document.body.appendChild(rootDiv)
        inputElem.focus()
      }
    // open the search if the user hits the `s` key when not focused on a text input
    document.body.addEventListener("keydown", (e: KeyboardEvent) => handleGlobalKeyDown(e))

    icon

  private val inputElem: html.Input =
    input(id := "scaladoc-searchbar-input").tap { element =>
      element.addEventListener("input", { e =>
        val inputValue = e.target.asInstanceOf[html.Input].value
        if inputValue.isEmpty then showHints()
        else handleNewQuery(inputValue)
      })

      element.autocomplete = "off"
    }

  private val resultsDiv: html.Div =
    div(id := "scaladoc-searchbar-results")

  private val rootHiddenClasses = "hidden"
  private val rootShowClasses   = ""

  private val rootDiv: html.Div =
    val element = div(id := "scaladoc-searchbar")(
      inputElem,
      resultsDiv
    ).tap { elem =>
      elem.addEventListener("mousedown", (e: Event) => e.stopPropagation())
      elem.addEventListener("keydown", {
        case e: KeyboardEvent =>
          if e.keyCode == 40 then handleArrowDown()
          else if e.keyCode == 38 then handleArrowUp()
          else if e.keyCode == 13 then handleEnter()
          else if e.keyCode == 27 then handleEscape()
      })
    }

    val rootParent = div(id := "searchbar-container")(
      element
    ).tap { rootElem =>
      rootElem.addEventListener("mousedown", (e: Event) => handleEscape())
    }

    searchIcon.addEventListener("mousedown", (e: Event) => e.stopPropagation())
    document.body.addEventListener("mousedown", (e: Event) =>
      if (document.body.contains(element)) {
        handleEscape()
      }
    )

    rootParent

  private def handleArrowUp() = {
    val selectedElement = resultsDiv.querySelector("[selected]")
    if selectedElement != null then {
      selectedElement.removeAttribute("selected")
      def recur(elem: raw.Element): raw.Element = {
        val prev = elem.previousElementSibling
        if prev == null then null
        else {
          if !prev.classList.contains("hidden") &&
            prev.classList.contains("scaladoc-searchbar-row") &&
            (prev.hasAttribute("result") || prev.hasAttribute("loadmore"))
          then prev
          else recur(prev)
        }
      }
      val sibling = recur(selectedElement)
      if sibling != null then {
        sibling.setAttribute("selected", "")
        resultsDiv.scrollTop = sibling.asInstanceOf[html.Element].offsetTop - (2 * sibling.asInstanceOf[html.Element].clientHeight)
      }
    }
  }
  private def handleArrowDown() = {
    val selectedElement = resultsDiv.querySelector("[selected]")
    def recur(elem: raw.Element): raw.Element = {
      val next = elem.nextElementSibling
      if next == null then null
      else {
        if !next.classList.contains("hidden") &&
          next.classList.contains("scaladoc-searchbar-row") &&
          (next.hasAttribute("result") || next.hasAttribute("loadmore"))
        then next
        else recur(next)
      }
    }
    if selectedElement != null then {
      val sibling = recur(selectedElement)
      if sibling != null then {
        selectedElement.removeAttribute("selected")
        sibling.setAttribute("selected", "")
        resultsDiv.scrollTop = sibling.asInstanceOf[html.Element].offsetTop - (2 * sibling.asInstanceOf[html.Element].clientHeight)
      }
    } else {
      val firstResult = resultsDiv.firstElementChild
      if firstResult != null then {
        val toSelect = if firstResult.classList.contains("scaladoc-searchbar-row") && firstResult.hasAttribute("result") then firstResult else recur(firstResult)
        toSelect.setAttribute("selected", "")
        resultsDiv.scrollTop = toSelect.asInstanceOf[html.Element].offsetTop - (2 * toSelect.asInstanceOf[html.Element].clientHeight)
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
    inputElem.value = ""
    showHints()
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
          window.setTimeout(() => inputElem.focus(), 1.0)
        }
      }
    }
  }

  private def showHints() = {
    def clearResults() = while (resultsDiv.hasChildNodes()) resultsDiv.removeChild(resultsDiv.lastChild)
    val hintsDiv = div(cls := "searchbar-hints")(
      span(cls := "fas fa-lightbulb fa-5x"),
      h1("A bunch of hints to make your life easier"),
      ul(cls := "searchbar-hints-list")(
        li("Type a phrase to search members ", b("by name")," and static sites ", b("by title"),""),
        li("Type abbreviations", b("cC, caCa, camCa")," to search for ", b("camelCase")),
        li(
          "Type a function signature to search for members ", b("by signature")," using Inkuire",
          ul(
            li("Type ", b("String => Int")," to find ", b("String.size"),", ", b("String.toInt"),""),
            li("Type ", b("String => String => String")," to find ", b("String.mkString"),", ", b("String.stripPrefix"),""),
            li("Inkuire also finds field accessors. Type ", b("Some[A] => A")," to find ", b("Some.value"),""),
            li("For more information about Inkuire see ", a(href := "https://docs.scala-lang.org/scala3/guides/scaladoc/search-engine.html")("the documentation")),
            li("The availability of this function depends on configuration used to generate Scaladoc")
          )
        )
      )
    )
    clearResults()
    resultsDiv.appendChild(hintsDiv)
  }

  showHints()
