scalaVersion := sys.props("plugin.scalaVersion")

// Make `console` non-interactive so it cannot hang CI: the init script runs
// and the REPL quits immediately.
Compile / console / scalacOptions ++= Seq(
  "--repl-init-script", """println("REPL_JLINE_OK")""",
  "--repl-quit-after-init",
)
