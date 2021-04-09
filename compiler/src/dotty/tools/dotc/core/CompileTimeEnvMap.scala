package dotty.tools.dotc.core

import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.config.Settings.Setting.value
import Contexts._

// TODO doc
final case class CompileTimeEnvMap(env: Map[String, String]) {
  def apply(key: String): Option[String] =
    env.get(key)
}

object CompileTimeEnvMap {

  def fromSettings(using Context): CompileTimeEnvMap = {
    var m = Map.empty[String, String]

    for (s <- ctx.settings.compileTimeEnv.value)
      val i = s.indexOf('=')
      if i > 0 then
        // -Ekey=value
        val key = s.take(i)
        val value = s.drop(i + 1)
        m = m.updated(key, value)
      else if i < 0 then
        // -Ekey
        val key = s
        if !m.contains(key) then m = m.updated(key, "")

    new CompileTimeEnvMap(m)
  }
}
