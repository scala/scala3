package scala.tasty.reflect

trait SettingsOps extends Core {

  /** Compiler settings */
  def settings: Settings

  trait SettingsAPI {
    /** Can print output using colors? */
    def color: Boolean
  }
  implicit def SettingsDeco(settings: Settings): SettingsAPI

}
