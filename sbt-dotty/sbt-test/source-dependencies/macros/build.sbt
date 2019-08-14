lazy val macros = project.in(file("macro"))
lazy val app = project.in(file("app")).dependsOn(macros)
