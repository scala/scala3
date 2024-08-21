// To be ran by Scala CLI (requires -with-compiler command line option)

@main def reportScalaVersion: Unit =
  println(dotty.tools.dotc.config.Properties.versionNumberString)
