import sbt._
import Keys._
import KeyRanks.DTask

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
      def printSummary(): Unit = println(problems.mkString("\n"))
      def problems: Array[Problem] = buffer.toArray
      def log(problem: Problem): Unit = buffer.append(problem)
      def comment(pos: xsbti.Position, msg: String): Unit = ()
    }

  lazy val checkSettings = Seq(
    Compile / compile / compilerReporter := reporter,
    check := (Compile / compile).failure.map(_ => {
      println(reporter.problems.toList)
      assert(reporter.problems.length == 1)
      val problem = reporter.problems.head
      // Check that all methods on position can ba called without crashing
      val pos = problem.position
      println(pos.sourceFile)
      println(pos.sourcePath)
      println(pos.line)
      println(pos.lineContent)
      println(pos.offset)
      println(pos.pointer)
      println(pos.pointerSpace)
    }).value
  )
}
