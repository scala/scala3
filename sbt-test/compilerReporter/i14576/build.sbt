import sbt.internal.util.ConsoleAppender

scalaVersion := sys.props("plugin.scalaVersion")

lazy val assertFeatureSummary = taskKey[Unit]("checks that feature warning summary is emitted")
lazy val assertNoFeatureSummary = taskKey[Unit]("checks that no feature warning summary is emitted")
lazy val assertDeprecationSummary = taskKey[Unit]("checks that deprecation warning summary is emitted")
lazy val assertNoDeprecationSummary = taskKey[Unit]("checks that no deprecation warning summary is emitted")
lazy val resetMessages = taskKey[Unit]("empties the messages list")

lazy val root = (project in file("."))
  .settings(
    scalacOptions += "-source:future",
    extraAppenders := { s => Seq(ConsoleAppender(FakePrintWriter)) },
    assertFeatureSummary := {
      assert {
        FakePrintWriter.messages.exists(_.contains("there were 2 feature warnings; re-run with -feature for details"))
      }
    },
    assertNoFeatureSummary := {
      assert {
        FakePrintWriter.messages.forall(!_.contains("; re-run with -feature for details"))
      }
    },
    assertDeprecationSummary := {
      assert {
        FakePrintWriter.messages.exists(_.contains("there were 3 deprecation warnings; re-run with -deprecation for details"))
      }
    },
    assertNoDeprecationSummary := {
      assert {
        FakePrintWriter.messages.forall(!_.contains("; re-run with -deprecation for details"))
      }
    },
    resetMessages := {
      FakePrintWriter.resetMessages
    },
  )
