ThisBuild / versionScheme := Some("early-semver")

ThisBuild / version := "5.1.0"

ThisBuild / scalaVersion := "3.1.3"

ThisBuild/scalacOptions ++= Seq("-unchecked", "-deprecation","-feature")

lazy val root = (project in file("."))
  .settings(
    name := "json-scala-values",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test,
    libraryDependencies += "org.scalacheck" % "scalacheck_2.13" % "1.16.0",
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.13.38",
    libraryDependencies += "dev.optics" % "monocle-core_3" % "3.1.0"
  )

val NEXUS_USERNAME = sys.props.get("NEXUS_USERNAME").getOrElse("user")
val NEXUS_PASSWORD = sys.props.get("NEXUS_PASSWORD").getOrElse("user")



credentials += Credentials("Sonatype Nexus Repository Manager",
                           "oss.sonatype.org",
                            NEXUS_USERNAME,
                            NEXUS_PASSWORD
                          )

credentials += Credentials(
  "GnuPG Key ID",
  "gpg",
  "DED8C8A2", // key identifier
  "ignored" // this field is ignored; passwords are supplied by pinentry
)

ThisBuild / organization := "com.github.imrafaelmerino"
ThisBuild / organizationName := "Rafael Merino García"
ThisBuild / organizationHomepage := Some(url("https://github.com/imrafaelmerino/imrafaelmerino.github.io"))

ThisBuild / scmInfo := Some(
    ScmInfo(
        url("https://github.com/imrafaelmerino/json-scala-values.git"),
        "git@github.com:imrafaelmerino/json-scala-values.git"
    )
)


ThisBuild / developers := List(
    Developer(
        id = "com.github.imrafaelmerino",
        name = "Rafael Merino García",
        email = "imrafael.merino@gmail.com",
        url = url("https://github.com/imrafaelmerino/imrafaelmerino.github.io")
    )
)

ThisBuild / description := "Declarative and immutable Json implemented with persistent data structures."
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/imrafaelmerino/json-scala-values"))

ThisBuild / publishTo :=
  {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }

ThisBuild / Test / parallelExecution := true

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true)