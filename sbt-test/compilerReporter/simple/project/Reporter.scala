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
      val problems = reporter.problems
      println(problems.toList)
      assert(problems.size == 1)

      // make sure position reported by zinc are proper
      val mainProblem = problems.head

      val line = mainProblem.position().line()
      assert(line.isPresent() == true)
      assert(line.get() == 9)

      val pointer = mainProblem.position().pointer()
      assert(pointer.isPresent() == true)
      assert(pointer.get() == 10)

      assert(problems.forall(_.position.offset.isPresent))

      assert(problems.count(_.severity == Severity.Error) == 1) // not found: er1,
    }).value
  )
}
