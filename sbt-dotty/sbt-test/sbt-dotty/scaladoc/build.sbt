scalaVersion := sys.props("plugin.scalaVersion")

lazy val assertUsingScaladoc = taskKey[Unit]("")

assertUsingScaladoc := {
  assert(useScaladoc.value)
}
