scalaVersion := sys.props("plugin.scalaVersion")

TaskKey[Unit]("assertUsingScaladoc") := {
  assert(useScaladoc.value, "Scaladoc version does not match the expected pattern")
}

TaskKey[Unit]("checkScaladocOptions") := {
  val options = (Compile / doc / scalacOptions).value
  assert(options.count(_ == "-project") == 1)
}

TaskKey[Unit]("checkHtmlFiles") := {
  val helloHtml = (Compile / doc / target).value / "api" / "hello.html"
  assert(helloHtml.exists)
}
