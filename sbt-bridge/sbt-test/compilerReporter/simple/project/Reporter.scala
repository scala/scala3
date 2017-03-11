import sbt._
import Keys._
import KeyRanks.DTask

object Reporter {
  import xsbti.{Reporter, Problem, Position, Severity, Maybe}

  lazy val check = TaskKey[Unit]("check", "make sure compilation info are forwared to sbt")

  // compilerReporter is marked private in sbt
  lazy val compilerReporter = TaskKey[Option[xsbti.Reporter]]("compilerReporter", "Experimental hook to listen (or send) compilation failure messages.", DTask)
  
  lazy val reporter = 
    Some(new xsbti.Reporter {
      private val buffer = collection.mutable.ArrayBuffer.empty[Problem]
      def reset(): Unit = buffer.clear()
      def hasErrors: Boolean = buffer.exists(_.severity == Severity.Error)
      def hasWarnings: Boolean = buffer.exists(_.severity == Severity.Warn)
      def printSummary(): Unit = println(problems.mkString(System.lineSeparator))
      def problems: Array[Problem] = buffer.toArray
      def log(pos: Position, msg: String, sev: Severity): Unit = {
        object MyProblem extends Problem {
          def category: String = null
          def severity: Severity = sev
          def message: String = msg
          def position: Position = pos
          override def toString = s"custom: $position:$severity: $message"
        }
        buffer.append(MyProblem)
      }
      def comment(pos: xsbti.Position, msg: String): Unit = ()
    })

  lazy val checkSettings = Seq(
    compilerReporter in (Compile, compile) := reporter,
    check <<= (compile in Compile).mapFailure( _ => {
      val problems = reporter.get.problems
      println(problems.toList)
      assert(problems.size == 2)
      assert(problems.forall(_.position.offset.isDefined))
      assert(problems.count(_.severity == Severity.Error) == 1) // not found: er1,
      assert(problems.count(_.severity == Severity.Warn) == 1)  // `with' as a type operator has been deprecated; use `&' instead,
    })
  )
}
