lazy val root = project.in(file(".")).dependsOn(a)
lazy val a = project.in(file("a"))
