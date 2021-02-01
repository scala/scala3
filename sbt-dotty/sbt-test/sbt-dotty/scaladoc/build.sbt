scalaVersion := sys.props("plugin.scalaVersion")

lazy val assertUsingscaladoc = taskKey[Unit]("")

assertUsingscaladoc := {
  assert(usescaladoc.value)
}
