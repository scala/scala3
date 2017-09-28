import sbt.{Project, ProjectReference, SettingsDefinition}

object Modes {

  sealed trait Mode

  object NonBootstrapped extends Mode
  object Bootstrapped extends Mode
  object BootstrappedOptimised extends Mode

  implicit class ProjectModesOps(val project: Project) extends AnyVal {

    /** Applies the settings if mode is not bootstrapped */
    def nonBootstrappedSettings(s: SettingsDefinition*)(implicit mode: Mode): Project =
      if (mode == NonBootstrapped) project.settings(s: _*) else project

    /** Applies the settings if mode is bootstrapped */
    def bootstrappedSettings(s: SettingsDefinition*)(implicit mode: Mode): Project =
      if (mode == NonBootstrapped) project else project.settings(s: _*)

    /** Aggregate only if the mode is bootstrapped */
    def bootstrappedAggregate(s: ProjectReference*)(implicit mode: Mode): Project =
      if (mode == NonBootstrapped) project else project.aggregate(s: _*)

  }
}
