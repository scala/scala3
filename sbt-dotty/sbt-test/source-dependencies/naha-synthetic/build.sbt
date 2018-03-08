lazy val root = (project in file(".")).
  settings(
    scalaVersion := "2.11.7",
    incOptions := incOptions.value.withIncludeSynthToNameHashing(true)
  )
