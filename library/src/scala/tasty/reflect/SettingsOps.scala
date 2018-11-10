package scala.tasty.reflect

trait SettingsOps extends Core {

  /** Compiler settings */
  def settings: Settings

  trait SettingsAPI {
    def color: Boolean
  }
  implicit def SettingsDeco(settings: Settings): SettingsAPI

}
