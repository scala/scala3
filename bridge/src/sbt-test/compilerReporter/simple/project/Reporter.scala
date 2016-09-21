import sbt._

object Reporter {
  import xsbti.{Reporter, Problem, Position, Severity, Maybe}

  val check = TaskKey[Unit]("check", "make sure compilation info are forwared to sbt")

  val reporter = 
    Some(new xsbti.Reporter {
      private val buffer = collection.mutable.ArrayBuffer.empty[Problem]
      def reset(): Unit = {
        println("reset")
        buffer.clear()
      }
      def hasErrors: Boolean = {
        println("hasErrors")
        buffer.exists(_.severity == Severity.Error)
      }
      def hasWarnings: Boolean = {
        println("hasWarnings")
        buffer.exists(_.severity == Severity.Warn)
      }
      def printSummary(): Unit = {
        println("printSummary")
        def toOption[T](m: Maybe[T]): Option[T] = {
          if(m.isEmpty) None
          else Some(m.get)
        }
        println(problems.mkString(System.lineSeparator))
      }
      def problems: Array[Problem] = {
        println("problems")
        println(buffer.toList)
        buffer.toArray
      }
      def log(pos: Position, msg: String, sev: Severity): Unit = {
        println("log")
        object MyProblem extends Problem {
          def category: String = null
          def severity: Severity = sev
          def message: String = msg
          def position: Position = pos
          override def toString = s"custom: $position:$severity: $message"
        }
        buffer.append(MyProblem)
      }
      def comment(pos: xsbti.Position, msg: String): Unit = {
        println("comment")
      }

    })

  val checkSettings = Seq(
    check := {
      val problems = reporter.get.problems

      assert(problems.size == 2)
    }
  )

  // compilerReporter is marked private in sbt
  val hack = TaskKey[Option[Reporter]]("compilerReporter", "")

  
}