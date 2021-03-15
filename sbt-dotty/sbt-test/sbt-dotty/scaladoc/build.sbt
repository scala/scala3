scalaVersion := sys.props("plugin.scalaVersion")

lazy val assertUsingScaladoc = taskKey[Unit]("")

assertUsingScaladoc := {
  assert(useScaladoc.value)
}

TaskKey[Unit]("checkScaladocOptions") := {
  val options = (doc / scalacOptions).value
  assert(options.count(_ == "-project") == 1)
}
