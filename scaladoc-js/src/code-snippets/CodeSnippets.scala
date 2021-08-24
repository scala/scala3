package dotty.tools.scaladoc

import scala.scalajs.js
import org.scalajs.dom._
import org.scalajs.dom.ext._

import CodeSnippetsGlobals._

class CodeSnippets:
  lazy val scastieConfig = getScastieConfiguration

  private def getScastieConfiguration: js.Dynamic =
    js.Dynamic.literal(
      sbtConfig = scastieConfiguration,
      targetType = "scala3"
    )

  private def getButtonsSection(snippet: html.Element): Option[html.Div] = snippet.querySelector("div.buttons") match {
    case div: html.Div => Some(div)
    case _ => None
  }

  def enrichSnippets() = document.querySelectorAll("div.snippet").foreach {
    case snippet: html.Element =>
      snippet.addEventListener("click", (e: MouseEvent) => e.asInstanceOf[js.Dynamic].fromSnippet = true)
      snippetAnchor(snippet)
      handleHideableCode(snippet)
      handleImportedCode(snippet)
      copyRunButtons(snippet)
  }

  private def handleHideableCode(snippet: html.Element): Unit = {
    def toggleHide(e: html.Element | html.Document) = e.querySelectorAll(".hideable").foreach {
      case e: html.Element => e.classList.toggle("hidden")
      case _ =>
    }
    def createShowHideButton(toggleRoot: html.Element) = {
      val div = document.createElement("div")
      div.classList.add("snippet-showhide")
      val p = document.createElement("p")
      p.textContent = "Show collapsed lines"
      val showHideButton = document.createElement("label")
      showHideButton.classList.add("snippet-showhide-button")
      val checkbox = document.createElement("input").asInstanceOf[html.Input]
      checkbox.`type` = "checkbox"
      val slider = document.createElement("span")
      slider.classList.add("slider")
      showHideButton.appendChild(checkbox)
      showHideButton.appendChild(slider)
      checkbox.addEventListener("change", _ => toggleHide(toggleRoot))
      div.appendChild(showHideButton)
      div.appendChild(p)
      div
    }

    toggleHide(snippet)
    val buttonsSection = getButtonsSection(snippet)
    val hideables = snippet.querySelectorAll(".hideable")
    if hideables != null && hideables.nonEmpty then {
      val showHideButton = createShowHideButton(snippet)
      buttonsSection.foreach(_.appendChild(showHideButton))
    }
  }

  private def snippetAnchor(snippet: html.Element): Unit = snippet.querySelector(".snippet-meta .snippet-label") match {
    case e: html.Element =>
      val name = e.textContent.trim
      val anchor = document.createElement("a").asInstanceOf[html.Anchor]
      anchor.id = s"snippet-$name"
      snippet.insertBefore(anchor, snippet.firstChild)
    case _ =>
  }

  private def handleImportedCode(snippet: html.Element): Unit = {
    val included = snippet.querySelectorAll("code span.include")
    val pre = snippet.querySelector("pre")
    if included != null && included.nonEmpty && pre != null then {
      val includesDiv = document.createElement("div")
      includesDiv.classList.add("included-section")
      includesDiv.classList.add("hideable")
      included
        .collect { case e: html.Element => e }
        .toList
        .filter(_.hasAttribute("name"))
        .map(_.getAttribute("name"))
        .distinct
        .map { name =>
          val a = document.createElement("a").asInstanceOf[html.Anchor]
          a.classList.add("unselectable")
          a.href = s"#snippet-$name"
          a.innerHTML = s"included <b>$name</b>"
          a
        }
        .foreach(a => includesDiv.appendChild(a))

      snippet.insertBefore(includesDiv, pre)
    }
  }

  private def copyRunButtons(snippet: html.Element) = {
    def copyButton = {
      val div = document.createElement("div")
      val button = document.createElement("button")
      val icon = document.createElement("i")
      icon.classList.add("far")
      icon.classList.add("fa-clone")
      button.appendChild(icon)
      button.classList.add("copy-button")
      button.addEventListener("click", _ => {
        val code = snippet.querySelectorAll("code>span:not(.hidden)")
          .map(_.textContent)
          .mkString
        window.navigator.clipboard.writeText(code)
      })
      div.appendChild(button)
      div
    }
    def runButton = {
      val div = document.createElement("div").asInstanceOf[html.Div]
      val runButton = document.createElement("button").asInstanceOf[html.Button]
      val runIcon = document.createElement("i")
      runIcon.classList.add("fas")
      runIcon.classList.add("fa-play")
      runButton.classList.add("run-button")
      runButton.appendChild(runIcon)

      runButton.addEventListener("click", _ =>
        if !runButton.hasAttribute("opened") then {
          scastie.Embedded(snippet.querySelector("pre"), scastieConfig)
          runButton.setAttribute("opened", "opened")
        }
        snippet.querySelector(".scastie .embedded-menu") match {
          case btn: html.Element =>
            btn.style = "display:none;"
          case _ =>
        }
        snippet.querySelector(".scastie .embedded-menu .run-button") match {
          case btn: html.Element => btn.click()
          case _ =>
        }
        snippet.querySelector(".buttons .exit-button") match {
          case btn: html.Element => btn.parentElement.style = ""
          case _ =>
        }
        snippet.querySelector(".buttons .to-scastie-button") match {
          case btn: html.Element => btn.parentElement.style = ""
          case _ =>
        }
      )

      div.appendChild(runButton)
      div
    }
    def exitButton = {
      val div = document.createElement("div").asInstanceOf[html.Div]
      val exitButton = document.createElement("button").asInstanceOf[html.Element]
      val exitIcon = document.createElement("i")
      exitIcon.classList.toggle("fas")
      exitIcon.classList.toggle("fa-times")
      exitButton.classList.add("exit-button")
      div.style = "display:none;"
      exitButton.appendChild(exitIcon)

      exitButton.addEventListener("click", _ =>
        snippet.querySelector("pre") match {
          case p: html.Element => p.style = ""
          case _ =>
        }
        snippet.querySelector(".scastie.embedded") match {
          case s: html.Element => snippet.removeChild(s)
          case _ =>
        }
        snippet.querySelector(".buttons .run-button") match {
          case btn: html.Element => btn.removeAttribute("opened")
          case _ =>
        }
        snippet.querySelector(".buttons .to-scastie-button") match {
          case btn: html.Element => btn.parentElement.style = "display:none;"
          case _ =>
        }
        div.style = "display:none;"
      )

      div.appendChild(exitButton)
      div
    }
    def toScastieButton = {
      val div = document.createElement("div").asInstanceOf[html.Div]
      val toScastieButton = document.createElement("button").asInstanceOf[html.Element]
      val toScastieIcon = document.createElement("i").asInstanceOf[html.Image]

      toScastieIcon.classList.add("fas")
      toScastieIcon.classList.add("fa-external-link-alt")
      toScastieButton.classList.add("to-scastie-button")
      div.style = "display:none;"
      toScastieButton.appendChild(toScastieIcon)

      toScastieButton.addEventListener("click", _ =>
        snippet.querySelector(".embedded-menu li.logo") match {
          case toScastie: html.Element => toScastie.click()
          case _ =>
        }
      )

      div.appendChild(toScastieButton)
      div
    }
    val buttonsSection = getButtonsSection(snippet)
    buttonsSection.foreach(s =>
      s.appendChild(copyButton)
      if !snippet.hasAttribute("hasContext") then {
        s.appendChild(toScastieButton)
        s.appendChild(runButton)
        s.appendChild(exitButton)
      }
    )
  }

  enrichSnippets()

