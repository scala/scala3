TaskKey[Unit]("output-empty") := Def.uncached {
  val outputDirectory = (Compile / classDirectory).value
  val classes = (outputDirectory ** "*.class").get
  if (classes.nonEmpty) sys.error("Classes existed:\n\t" + classes.mkString("\n\t")) else ()
}
