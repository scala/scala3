/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc.tastyreflect

trait SettingsOpsImpl extends scala.tasty.reflect.SettingsOps with scala.tasty.reflect.ContextOps with CoreImpl {

  def settings: Settings = rootContext.settings

  def SettingsDeco(settings: Settings): SettingsAPI = new SettingsAPI {
    def color: Boolean = settings.color.value == "always"
  }

}
