package dotty.tools.dotc.tastyreflect

trait SettingsOpsImpl extends scala.tasty.reflect.SettingsOps with scala.tasty.reflect.ContextOps with CoreImpl {

  def settings: Settings = rootContext.settings

  def SettingsDeco(settings: Settings): SettingsAPI = new SettingsAPI {
    def color: Boolean = settings.color.value == "always"
  }

}
