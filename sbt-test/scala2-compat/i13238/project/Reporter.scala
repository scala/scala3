import sbt._
import Keys._
import KeyRanks.DTask

object Reporter {
  import xsbti.{Reporter, Problem, Position, Severity}

  lazy val check = TaskKey[Unit]("check", "Check compilation output")

  // compilerReporter is marked private in sbt
  lazy val compilerReporter = TaskKey[xsbti.Reporter]("compilerReporter", "Experimental hook to listen (or send) compilation failure messages.", DTask)

  lazy val reporter =
    new xsbti.Reporter {
      private val buffer = collection.mutable.ArrayBuffer.empty[Problem]
      def reset(): Unit = buffer.clear()
      def hasErrors: Boolean = buffer.exists(_.severity == Severity.Error)
      def hasWarnings: Boolean = buffer.exists(_.severity == Severity.Warn)
      def printSummary(): Unit = println(problems.mkString("\n"))
      def problems: Array[Problem] = buffer.toArray
      def log(problem: Problem): Unit = buffer.append(problem)
      def comment(pos: xsbti.Position, msg: String): Unit = ()
    }

  lazy val checkSettings = Seq(
    Compile / compile / compilerReporter := reporter,
    check := (Compile / compile).failure.map(_ => {
      val problems = reporter.problems
      assert(problems.size == 1, problems.size)
      assert(problems.head.position.line.get() == 5, problems.head.position.line)
    }).value
  )
}
