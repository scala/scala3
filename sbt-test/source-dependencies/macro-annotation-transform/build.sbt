name := "macro-annotation-transform"
scalacOptions += "-experimental"
Compile / sourceGenerators += Def.task {
  val file = (Compile / sourceManaged).value / "OutputFile.scala"
  IO.write(
    file,
    s"""
      |package example
      |val outputFile = java.nio.file.Paths.get("${baseDirectory.value}").resolve("output").resolve("actual.txt")
      |""".stripMargin.trim
  )
  Seq(file)
}
