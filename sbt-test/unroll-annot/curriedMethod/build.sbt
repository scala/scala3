lazy val utils = project.in(file("utils"))

lazy val sharedSettings = Seq(
  scalacOptions ++= Seq("-Ycheck:all", "-experimental")
)

lazy val v1 = project.in(file("v1"))
  .settings(sharedSettings)

lazy val v1_app = project.in(file("v1_app")).dependsOn(utils)
  .settings(sharedSettings)
  .settings(
    Compile / unmanagedClasspath := Seq(
      Attributed.blank((v1 / Compile / classDirectory).value)
    ),
  )

lazy val v2 = project.in(file("v2"))
  .settings(sharedSettings)

lazy val v2_app = project.in(file("v2_app")).dependsOn(utils)
  .settings(sharedSettings)
  .settings(
    Runtime / unmanagedClasspath := Seq(
      // add v1_app, compiled against v1, to the classpath
      Attributed.blank((v1_app / Runtime / classDirectory).value)
    ),
    Compile / unmanagedClasspath := Seq(
      Attributed.blank((v2 / Compile / classDirectory).value)
    ),
  )

lazy val v3 = project.in(file("v3"))
  .settings(sharedSettings)

lazy val v3_app = project.in(file("v3_app")).dependsOn(utils)
  .settings(sharedSettings)
  .settings(
    Runtime / unmanagedClasspath := Seq(
      // add v1_app, compiled against v1, to the classpath
      Attributed.blank((v1_app / Runtime / classDirectory).value),
      // add v2_app, compiled against v2, to the classpath
      Attributed.blank((v2_app / Runtime / classDirectory).value),
    ),
    Compile / unmanagedClasspath := Seq(
      Attributed.blank((v3 / Compile / classDirectory).value)
    ),
  )
