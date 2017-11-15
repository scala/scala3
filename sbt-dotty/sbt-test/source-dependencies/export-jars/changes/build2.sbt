lazy val root = Project("root", file(".")) dependsOn(a)
lazy val a = Project("a", file("a"))

exportJars := true