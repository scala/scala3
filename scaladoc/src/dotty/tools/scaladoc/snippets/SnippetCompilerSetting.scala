package dotty.tools.scaladoc
package snippets

import dotty.tools.scaladoc.DocContext
import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.ScalaSettings

case class SnippetCompilerSetting[T](setting: Setting[T], value: T)
