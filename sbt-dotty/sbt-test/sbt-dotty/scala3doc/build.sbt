scalaVersion := sys.props("plugin.scalaVersion")

lazy val assertUsingScala3doc = taskKey[Unit]("")

assertUsingScala3doc := {
  assert(useScala3doc.value)
}
