package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

class CodeSnippets:

  private def getButtonsSection(snippet: html.Element): Option[html.Div] = snippet.querySelector("div.buttons") match {
    case div: html.Div => Some(div)
    case _ => None
  }

  def enrichSnippets() = document.querySelectorAll("div.snippet").foreach {
    case snippet: html.Element =>
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
      val div = document.createElement("div")
      val button = document.createElement("button").asInstanceOf[html.Button]
      val icon = document.createElement("i")
      icon.classList.add("fas")
      icon.classList.add("fa-play")
      button.appendChild(icon)
      button.classList.add("run-button")
      button.addEventListener("click", _ => {}) // TODO: Run button #13065
      button.disabled = true
      div.appendChild(button)
      div
    }
    val buttonsSection = getButtonsSection(snippet)
    buttonsSection.foreach(s =>
      s.appendChild(copyButton)
      s.appendChild(runButton)
    )
  }

  enrichSnippets()

