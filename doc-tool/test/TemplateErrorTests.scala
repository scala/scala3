package dotty.tools
package dottydoc
package staticsite

import org.junit.Test
import org.junit.Assert._

class TemplateErrorTests extends DottyDocTest with SourceFileOps {
  @Test def unclosedTag: Unit = {
    htmlPage(
      """|Yo dawg:
         |{% include "stuff"
         |I heard you like to include stuff""".stripMargin
    ).html
  }

  @Test def missingEndif: Unit = {
    htmlPage(
      """|{% if someStuff %}
         |Dude
         |""".stripMargin
    ).html
  }

  @Test def nonExistingTag: Unit = {
    htmlPage(
      """|{% someStuff 'ofDude' %}
         |Dude
         |""".stripMargin
    ).html
  }
}
