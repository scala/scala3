package scala.tasty.reflect

trait SettingsOps extends ReflectionCore {

  /** Compiler settings */
  def settings: Settings

  trait SettingsAPI {
    def color: Boolean
  }
  implicit def SettingsDeco(settings: Settings): SettingsAPI

}
