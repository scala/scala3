scalaVersion := sys.props("plugin.scalaVersion")

TaskKey[Unit]("checkScaladocOptions") := {
  val options = (Compile / doc / scalacOptions).value
  assert(options.count(_ == "-project") == 1)
}

TaskKey[Unit]("checkHtmlFiles") := {
  val helloHtml = (Compile / doc / target).value / "api" / "hello.html"
  assert(helloHtml.exists)
}
