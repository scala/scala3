
scalaVersion := sys.props("plugin.scalaVersion")

lazy val dependencies = Seq(
  "org.jooq" % "jooq-codegen" % "3.18.7",
)

lazy val jooqtest = (project in file("."))
    .settings(libraryDependencies ++= dependencies)
