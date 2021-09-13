package dotty.tools.scaladoc
package snippets

import dotty.tools.scaladoc.DocContext
import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.{ ScalaSettings, ScalaVersion }

case class SnippetCompilerSetting[T](setting: Setting[T], value: T):
  def toArgs: List[String] = value match
    case v: Boolean => List(setting.name)
    case v: List[_] => List(s"${setting.name}:${v.map(_.toString).mkString(",")}")
    case v: ScalaVersion => List(s"${setting.name}", s"${v.unparse}")
    case v => List(s"${setting.name}", s"$v")
