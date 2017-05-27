TaskKey[Unit]("output-empty") <<= classDirectory in Configurations.Compile map { outputDirectory =>
	def classes = (outputDirectory ** "*.class").get
	if(!classes.isEmpty) sys.error("Classes existed:\n\t" + classes.mkString("\n\t")) else ()
}

// apparently Travis CI stopped allowing long file names
// it fails with the default setting of 255 characters so
// we have to set lower limit ourselves
scalacOptions ++= Seq("-Xmax-classfile-name", "240")
