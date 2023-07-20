import sbt._
import Keys._
import KeyRanks.DTask

import scala.jdk.CollectionConverters.*

object Reporter {
  import xsbti.{Reporter, Problem, Position, Severity}

  lazy val check = TaskKey[Unit]("check", "make sure compilation info are forwared to sbt")

  // compilerReporter is marked private in sbt
  lazy val compilerReporter = TaskKey[xsbti.Reporter]("compilerReporter", "Experimental hook to listen (or send) compilation failure messages.", DTask)

  lazy val reporter =
    new xsbti.Reporter {
      private val buffer = collection.mutable.ArrayBuffer.empty[Problem]
      def reset(): Unit = buffer.clear()
      def hasErrors: Boolean = buffer.exists(_.severity == Severity.Error)
      def hasWarnings: Boolean = buffer.exists(_.severity == Severity.Warn)
      def printSummary(): Unit = println(problems.mkString(System.lineSeparator))
      def problems: Array[Problem] = buffer.toArray
      def log(problem: Problem): Unit = buffer.append(problem)
      def comment(pos: xsbti.Position, msg: String): Unit = ()
    }

  lazy val checkSettings = Seq(
    Compile / compile / compilerReporter := reporter,
    check := (Compile / compile).failure.map(_ => {
      val problems = reporter.problems
      println(problems.toList)

      problems match {
        case Array(err, warning) =>
          // Checking the error reported
          val eline = err.position().line()
          assert(eline.isPresent() == true)
          assert(eline.get() == 9)

          val ediagnosticCode = err.diagnosticCode()
          assert(ediagnosticCode.isPresent() == true)
          val ecode = ediagnosticCode.get().code()
          assert(ecode == "6")

          val epointer = err.position().pointer()
          assert(epointer.isPresent() == true)
          assert(epointer.get() == 10)

          assert(err.position.offset.isPresent)

          assert(err.severity == Severity.Error) // not found: er1,

          // Checking the warning reported
   
          val wline = warning.position().line()
          assert(wline.isPresent() == true)
          assert(wline.get() == 12)

          val wdiagnosticCode = warning.diagnosticCode()
          assert(wdiagnosticCode.isPresent() == true)
          val wcode = wdiagnosticCode.get().code()
          assert(wcode == "99")

          val wpointer = warning.position().pointer()
          assert(wpointer.isPresent() == true)
          assert(wpointer.get() == 12)

          assert(warning.position.offset.isPresent)

          assert(warning.severity == Severity.Warn) // Only function types can be followed by _ but the current expression has type Int

          val actions = warning.actions().asScala.toList

          assert(actions.size == 1)

          val action = actions.head

          assert(action.title() == "Rewrite to function value")

          val edits = action.edit().changes().asScala.toList

          assert(edits.size == 2)

        case somethingElse =>
          assert(false, s"Only expected to have a single error and a single warning, but instead got: ${somethingElse.toString}")

      }
    }).value
  )
}
