package dotty.tools.scaladoc

import scala.scalajs.js
import org.scalajs.dom._
import org.scalajs.dom.ext._

import utils.HTML._
import scala.util.chaining._

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

  def enrichSnippets() = document.querySelectorAll("div.snippet[scala-snippet]").foreach {
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
      div(cls := "snippet-showhide-container")(
        label(cls := "snippet-showhide-button")(
          input("type" := "checkbox", cls := "snippet-showhide").tap(_.addEventListener("change", _ => toggleHide(toggleRoot))),
        ),
      )
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
      val anchor = a(id := s"snippet-$name")
      snippet.insertBefore(anchor, snippet.firstChild)
    case _ =>
  }

  private def handleImportedCode(snippet: html.Element): Unit = {
    val included = snippet.querySelectorAll("code span.include")
    val pre = snippet.querySelector("pre")
    if included != null && included.nonEmpty && pre != null then {
      val includes = included
        .collect { case e: html.Element => e }
        .toList
        .filter(_.hasAttribute("name"))
        .map(_.getAttribute("name"))
        .distinct
        .map { name =>
          a(cls := "unselectable", href := s"#snippet-$name")(
            "included",
            b(name)
          )
        }

      val includesDiv = div(cls := "included-section hideable")(includes)

      snippet.insertBefore(includesDiv, pre)
    }
  }

  private def copyRunButtons(snippet: html.Element) = {
    val copyButtonIcon = s"""<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">
<path d="M2.5 2.5V9.5H4V11H2C1.44772 11 1 10.5523 1 10V2C1 1.44772 1.44772 1 2 1H10C10.5523 1 11 1.44772 11 2V4H9.5V2.5H2.5Z" fill="#A09FA6"/>
<path fill-rule="evenodd" clip-rule="evenodd" d="M5 6C5 5.44772 5.44772 5 6 5H14C14.5523 5 15 5.44772 15 6V14C15 14.5523 14.5523 15 14 15H6C5.44772 15 5 14.5523 5 14V6ZM6.5 13.5V6.5H13.5V13.5H6.5Z" fill="#A09FA6"/>
</svg>
 """
    def copyButton = {
      div(
        button(cls := "copy-button icon-button").tap(_.addEventListener("click", _ => {
          val code = snippet.querySelectorAll("code>span:not(.hidden)")
            .map(_.textContent)
            .mkString
          window.navigator.clipboard.writeText(code)
        }))
      )
    }
    def runButton = {
      val runButton = button(cls := "run-button")(
        i(cls := "fas fa-play")
      )
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

      div(runButton)
    }
    def exitButton = {
      val exitButton = button(cls := "exit-button")(
        i(cls := "fas fa-times")
      )

      val bdiv = div(style := "display:none;")(exitButton)

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
        bdiv.style = "display:none;"
      )

      bdiv
    }

    def toScastieButton = {
      val toScastieButton = button(cls := "to-scastie-button")(
        i(cls := "fas fa-external-link-alt")
      )

      toScastieButton.addEventListener("click", _ =>
        snippet.querySelector(".embedded-menu li.logo") match {
          case toScastie: html.Element => toScastie.click()
          case _ =>
        }
      )

      div("style" := "display:none;")(toScastieButton)
    }

    val buttonsSection = getButtonsSection(snippet)
    buttonsSection.foreach(s =>
      s.appendChild(copyButton)
      if snippet.hasAttribute("runnable") then {
        s.appendChild(toScastieButton)
        s.appendChild(runButton)
        s.appendChild(exitButton)
      }
    )
  }

  enrichSnippets()

