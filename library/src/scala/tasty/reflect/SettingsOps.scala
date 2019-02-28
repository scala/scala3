package scala.tasty.reflect

trait SettingsOps extends Core {

  /** Compiler settings */
  def settings: Settings = kernel.settings

  implicit class SettingsAPI(self: Settings) {
    /** Can print output using colors? */
    def color: Boolean = kernel.Settings_color(self)
  }

}
