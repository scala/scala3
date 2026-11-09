import sbt.internal.util.ConsoleAppender

// Reproduces https://github.com/scala/scala3/issues/20010
//
// Three modules:
//   a  : defines `parent.ParsingTest`
//   b  : defines `child.ValidatingTest extends parent.ParsingTest`,
//        compiled with `a` on the classpath; its output ends up in `c-input`
//   c  : root project that extends `child.ValidatingTest`, but only sees
//        `c-input` on its classpath – `a`'s outputs are deliberately not
//        exposed, so loading `ValidatingTest`'s TASTy can no longer resolve
//        its `parent.ParsingTest` parent.
//
// Before the fix the compiler crashed with
//   `java.lang.AssertionError: class ValidatingTest has non-class parent: ...`
// Now it must emit a clean `Bad symbolic reference` error.

lazy val assertCleanMissingRefError = taskKey[Unit](
  "checks that compiling c reports `Bad symbolic reference` rather than crashing"
)
lazy val resetMessages = taskKey[Unit]("empties the messages list")

lazy val a = project.in(file("a"))
  .settings(
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "a-only"
  )

lazy val b = project.in(file("b"))
  .settings(
    Compile / unmanagedClasspath += (ThisBuild / baseDirectory).value / "a-only",
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-input"
  )

lazy val c = project.in(file("."))
  .settings(
    // Only `b`'s outputs are visible here – `a-only` is *not* on the classpath.
    Compile / unmanagedClasspath += (ThisBuild / baseDirectory).value / "c-input",
    Compile / classDirectory := (ThisBuild / baseDirectory).value / "c-output",
    extraAppenders := { _ => Seq(ConsoleAppender(FakePrintWriter)) },
    resetMessages := { FakePrintWriter.resetMessages },
    assertCleanMissingRefError := {
      val msgs = FakePrintWriter.messages
      assert(
        msgs.exists(_.contains("Bad symbolic reference")),
        s"expected 'Bad symbolic reference' in compiler output, got: ${msgs.mkString("\n")}"
      )
      assert(
        !msgs.exists(_.contains("non-class parent")),
        s"compiler crashed with 'non-class parent' assertion; got: ${msgs.mkString("\n")}"
      )
      assert(
        !msgs.exists(_.contains("java.lang.AssertionError")),
        s"compiler crashed with AssertionError; got: ${msgs.mkString("\n")}"
      )
    }
  )
