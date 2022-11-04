package dotty.tools.scaladoc

import utils.HTML._

import scala.scalajs.js.Date
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
    def toHTML(boldChars: Set[Int]) =
      val location = if (p.isLocationExternal) {
        p.location
      } else {
        Globals.pathToRoot + p.location
      }

      val extensionTargetMessage = if (p.extensionTarget.isEmpty()) {
        ""
      } else {
        " extension on " + p.extensionTarget
      }

      div(cls := "scaladoc-searchbar-row mono-small-inline", "result" := "")(
        a(href := location)(
          p.fullName.zipWithIndex.map((c, i) => if boldChars.contains(i) then b(c.toString) else c.toString),
          span(i(extensionTargetMessage)),
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

      div(cls := "scaladoc-searchbar-row mono-small-inline", "result" := "", "inkuire-result" := "", "mq" := m.mq.toString)(
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

  extension (rq: RecentQuery)
    def toHTML =
      div(cls := "scaladoc-searchbar-row mono-small-inline", "result" := "")(
        a(
          span(rq.query)
        )
      ).tap { _.addEventListener("click", _ => {
          inputElem.value = rq.query
          inputElem.dispatchEvent(new Event("input"))
        })
      }.tap { wrapper => wrapper.addEventListener("mouseover", {
          case e: MouseEvent => handleHover(wrapper)
        })
      }

  def createKindSeparator(kind: String, customClass: String = "") =
    div(cls := "scaladoc-searchbar-row mono-small-inline", "divider" := "")(
      span(cls := s"micon ${kind.take(2)} $customClass"),
      span(kind)
    )

  def handleNewFluffQuery(matchers: List[Matchers]) =
    val result: List[(PageEntry, Set[Int])] = engine.query(matchers)
    val fragment = document.createDocumentFragment()
    def createLoadMoreElement =
      div(cls := "scaladoc-searchbar-row mono-small-inline", "loadmore" := "")(
        a(
          span("Load more")
        )
      ).tap { loadMoreElement => loadMoreElement
        .addEventListener("mouseover", _ => handleHover(loadMoreElement))
      }

    result.groupBy(_._1.kind).map {
      case (kind, entries) =>
        val kindSeparator = createKindSeparator(kind)
        val htmlEntries = entries.map((p, set) => p.toHTML(set))
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
    resultsDiv.appendChild(fragment)

  def handleRecentQueries(query: String) = {
    val recentQueries = RecentQueryStorage.getData
    if query != "" then RecentQueryStorage.addEntry(RecentQuery(query, Date.now()))
    val matching = recentQueries
      .filterNot(rq => rq.query.equalsIgnoreCase(query)) // Don't show recent query that is equal to provided query
      .filter { rq => // Fuzzy search
        rq.query.foldLeft(query) { (pattern, nextChar) =>
          if !pattern.isEmpty then {
            if pattern.head.toString.equalsIgnoreCase(nextChar.toString) then pattern.tail else pattern
          } else ""
      }.isEmpty
    }
    if matching.nonEmpty then {
      resultsDiv.appendChild(createKindSeparator("Recently searched", "fas fa-clock re-icon"))
      matching.map(_.toHTML).foreach(resultsDiv.appendChild)
    }
  }

  def createLoadingAnimation: raw.HTMLElement =
    div(cls := "loading-wrapper")(
      div(cls := "loading")
    )
  extension (s: String)
    def toHTMLError =
      div(cls := "scaladoc-searchbar-row mono-small-inline", "error" := "")(
        span(cls := "search-error")(s)
      )

  var timeoutHandle: SetTimeoutHandle = null
  def handleNewQuery(query: String) =
    resultsDiv.scrollTop = 0
    resultsDiv.onscroll = (event: Event) => { }
    val fragment = document.createDocumentFragment()
    timeoutHandle = setTimeout(600.millisecond) {
      clearResults()
      handleRecentQueries(query)
      parser.parse(query) match {
        case EngineMatchersQuery(matchers) =>
            handleNewFluffQuery(matchers)
        case BySignature(signature) =>
            val loading = createLoadingAnimation
            val kindSeparator = createKindSeparator("inkuire")
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

  private val mobileSearch =
    val mobileSearch = document.getElementById("mobile-scaladoc-searchbar-input").asInstanceOf[html.Input]
    mobileSearch.onfocus = (event: Event) =>
      if (document.body.contains(rootDiv)) {
        // document.body.removeChild(rootDiv)
      }
      else {
        document.body.appendChild(rootDiv)
        inputElem.focus()
      }
    // open the search if the user hits the `s` key when not focused on a text input
    document.body.addEventListener("keydown", (e: KeyboardEvent) => handleGlobalKeyDown(e))

  private val inputElem: html.Input =
    input(cls := "scaladoc-searchbar-input", `type` := "search", `placeholder`:= "Find anything").tap { element =>
      element.addEventListener("input", { e =>
        clearTimeout(timeoutHandle)
        val inputValue = e.target.asInstanceOf[html.Input].value
        if inputValue.isEmpty then {
          clearResults()
          if RecentQueryStorage.isEmpty then showHints() else handleRecentQueries("")
        } else handleNewQuery(inputValue)
      })

      element.autocomplete = "off"
    }

  private val resultsDiv: html.Div =
    div(id := "scaladoc-searchbar-results")

  def clearResults() = while (resultsDiv.hasChildNodes()) resultsDiv.removeChild(resultsDiv.lastChild)

  private val rootHiddenClasses = "hidden"
  private val rootShowClasses   = ""

  private def generateRootDiv(): html.Div =
    val cancelButton = span(cls := "scaladoc-searchbar-cancel-button body-small")("Cancel")
    cancelButton.onclick = (event: Event) =>
      document.body.removeChild(rootDiv)

    val inputContainer = div(cls := "scaladoc-searchbar-input-container")(
      inputElem,
      cancelButton,
    )

    val element = div(id := "scaladoc-searchbar")(
      inputContainer,
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

    val searchbarFooter = div(id := "searchbar-footer", cls := "body-small")(
      span(cls := "searchbar-footer-left-container")(
        span("Smart search:"),
        span(b("CC "), "to find CamcelCase phrases"),
        span(b("A=>B "), "to find CamcelCase signatures"),
      ),
      span(cls := "searchbar-footer-right-container")(
        span(b("Esc "), "to close"),
        span(b("Arrows "), "to navigate"),
        span(b("Enter "), "to select"),
      ),
    )

    val rootParent = div(id := "searchbar-container")(
      element,
      searchbarFooter
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

  private val rootDiv: html.Div = generateRootDiv()

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
    inputElem.dispatchEvent(new Event("input"))
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
    val hintsDiv = div(cls := "searchbar-hints")(
      span(cls := "lightbulb"),
      h1(cls := "body-medium")("A bunch of search hints to make your life easier"),
      ul(cls := "searchbar-hints-list")(
        h1(cls := "h100")("Members or Static sites by any phrase"),
        li(cls := "mono-small-inline")("Any phrase to find", b(" Name")," or ", b("Title"),""),
        div(cls := "divider"),
        li(cls := "mono-small-inline")("cC, caCa, camCa" , b(" to find")," camelCase"),
        h1(cls := "h100")("Members by signature"),
        li(cls := "mono-small-inline")("String => Int", b(" to find"), " String.size, String.toInt"),
        div(cls := "divider"),
        li(cls := "mono-small-inline")("String => String => String", b(" to find "), "String.mkString, String.stripPrefix"),
        div(cls := "divider"),
        li(cls := "mono-small-inline")("Some[A] => A", b(" to find"), " Some.value"),
        li(cls := "link body-small")("Availability of searching by inkuire depends on the configuration of Scaladoc. For more info, ", a(href := "https://docs.scala-lang.org/scala3/guides/scaladoc/search-engine.html")("the documentation")),
      )
    )
    resultsDiv.appendChild(hintsDiv)
  }

  inputElem.dispatchEvent(new Event("input"))
