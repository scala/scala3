package dotty.tools
package dotc.interactive

import dotc.ast.tpd
import dotc.{CompilationUnit, Compiler, Run}
import dotc.core.Contexts.Context
import dotc.core.Mode
import dotc.reporting.StoreReporter
import dotc.util.{SourceFile, SourcePosition}
import dotc.util.Spans.Span

import org.junit.Test

class CustomCompletionTests extends DottyTest:

   private def completions(
     input: String,
     dependencyCompleter: Option[String => (Int, Seq[String])] = None,
     deep: Boolean = false,
     extraDefinitions: String = ""
   ): (Int, Seq[Completion]) =
      val prefix = extraDefinitions + """
        object Wrapper {
          val expr = {
      """
      val suffix = """
          }
        }
      """

      val allCode = prefix + input + suffix
      val index = prefix.length + input.length

      val run = new Run(
        new Compiler,
        initialCtx.fresh
          .addMode(Mode.ReadPositions | Mode.Interactive)
          // discard errors - comment out this line to print them in the console
          .setReporter(new StoreReporter(null))
          .setSetting(initialCtx.settings.YstopAfter, List("typer"))
      )
      val file = SourceFile.virtual("<completions>", allCode, maybeIncomplete = true)
      given ctx: Context = run.runContext.withSource(file)
      val unit = CompilationUnit(file)
      ctx
        .run.nn
        .compileUnits(unit :: Nil, ctx)

      // ignoring compilation errors here - the input code
      // to complete likely doesn't compile

      unit.tpdTree = {
        import tpd._
        unit.tpdTree match {
          case PackageDef(_, p) =>
            p.reverseIterator.collectFirst {
              case TypeDef(_, tmpl: Template) =>
                tmpl.body
                  .collectFirst { case dd: ValDef if dd.name.show == "expr" => dd }
                  .getOrElse(sys.error("Unexpected tree shape"))
            }
            .getOrElse(sys.error("Unexpected tree shape"))
          case _ => sys.error("Unexpected tree shape")
        }
      }
      val ctx1 = ctx.fresh.setCompilationUnit(unit)
      val srcPos = SourcePosition(file, Span(index))
      val (offset0, completions) =
        if (deep || dependencyCompleter.nonEmpty)
          CustomCompletion.completions(srcPos, dependencyCompleteOpt = dependencyCompleter, enableDeep = deep)(using ctx1)
        else
          Completion.completions(srcPos)(using ctx1)
      val offset = offset0 - prefix.length
      (offset, completions)


   @Test def simple(): Unit =
      val prefix = "scala.collection.immutable."
      val input = prefix + "Ma"

      val (offset, completions0) = completions(input)
      val labels = completions0.map(_.label)

      assert(offset == prefix.length)
      assert(labels.contains("Map"))

   @Test def custom(): Unit =
      val prefix = "import $ivy."
      val input = prefix + "scala"

      val dependencies = Seq(
        "scalaCompiler",
        "scalaLibrary",
        "other"
      )
      val (offset, completions0) = completions(
        input,
        dependencyCompleter = Some { dep =>
          val matches = dependencies.filter(_.startsWith(dep))
          (0, matches)
        }
      )
      val labels = completions0.map(_.label)

      assert(offset == prefix.length)
      assert(labels.contains("scalaCompiler"))
      assert(labels.contains("scalaLibrary"))
      assert(labels.length == 2)

   @Test def backTicks(): Unit =
      val prefix = "Foo."
      val input = prefix + "a"

      val extraDefinitions =
        """object Foo { def a1 = 2; def `a-b` = 3 }
          |""".stripMargin
      val (offset, completions0) = completions(
        input,
        extraDefinitions = extraDefinitions,
        deep = true // Enables CustomCompleter
      )
      val labels = completions0.map(_.label)

      assert(offset == prefix.length)
      assert(labels.contains("a1"))
      assert(labels.contains("`a-b`"))

   @Test def backTicksDependencies(): Unit =
      val prefix = "import $ivy."
      val input = prefix + "`org.scala-lang:scala-`"

      val dependencies = Seq(
        "org.scala-lang:scala-compiler",
        "org.scala-lang:scala-library",
        "other"
      )
      val (offset, completions0) = completions(
        input,
        dependencyCompleter = Some { dep =>
          val matches = dependencies.filter(_.startsWith(dep))
          (0, matches)
        }
      )
      val labels = completions0.map(_.label)

      // Seems backticks mess with that for now...
      // assert(offset == prefix.length)
      assert(labels.contains("`org.scala-lang:scala-compiler`"))
      assert(labels.contains("`org.scala-lang:scala-library`"))
      assert(labels.length == 2)

   @Test def deep(): Unit =
      val prefix = ""
      val input = prefix + "ListBuf"

      val (offset, completions0) = completions(input, deep = true)
      val labels = completions0.map(_.label)

      assert(offset == prefix.length)
      assert(labels.contains("scala.collection.mutable.ListBuffer"))

   @Test def deepType(): Unit =
      val prefix = ""
      val input = prefix + "Function2"

      val (offset, completions0) = completions(input, deep = true)
      val labels = completions0.map(_.label)

      assert(offset == prefix.length)
      assert(labels.contains("scala.Function2"))

