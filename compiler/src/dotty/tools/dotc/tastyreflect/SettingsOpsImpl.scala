package dotty.tools.dotc.tastyreflect

trait SettingsOpsImpl extends scala.tasty.reflect.SettingsOps with TastyCoreImpl {

  def settings(implicit ctx: Context): Settings = ctx.settings

  def SettingsDeco(settings: Settings): SettingsAPI = new SettingsAPI {
    def color(implicit ctx: Context): Boolean = settings.color.value == "always"
  }

}
