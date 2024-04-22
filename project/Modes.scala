import sbt.{Project, ProjectReference, SettingsDefinition, Plugins}

object Modes {

  sealed trait Mode

  object NonBootstrapped extends Mode
  object Bootstrapped extends Mode

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

    /** Depends only if the mode is bootstrapped */
    def bootstrappedDependsOn(s: sbt.ClasspathDep[ProjectReference]*)(implicit mode: Mode): Project =
      if (mode == NonBootstrapped) project else project.dependsOn(s: _*)

    /** Plugins only if the mode is bootstrapped */
    def bootstrappedEnablePlugins(ns: Plugins*)(implicit mode: Mode): Project =
      if (mode == NonBootstrapped) project else project.enablePlugins(ns: _*)

  }
}
