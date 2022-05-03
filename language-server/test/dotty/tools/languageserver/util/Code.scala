package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.embedded._

/**
 * Helper object to create virtual source files and extract markers from the source files, using the
 * `code` interpolator.
 *
 * The markers can then be used to perform tests at different positions within the source
 * file.
 */
object Code {

  // Default positions
  val m1 = new CodeMarker("m1")
  val m2 = new CodeMarker("m2")
  val m3 = new CodeMarker("m3")
  val m4 = new CodeMarker("m4")
  val m5 = new CodeMarker("m5")
  val m6 = new CodeMarker("m6")
  val m7 = new CodeMarker("m7")
  val m8 = new CodeMarker("m8")
  val m9 = new CodeMarker("m9")
  val m10 = new CodeMarker("m10")
  val m11 = new CodeMarker("m11")
  val m12 = new CodeMarker("m12")
  val m13 = new CodeMarker("m13")
  val m14 = new CodeMarker("m14")
  val m15 = new CodeMarker("m15")
  val m16 = new CodeMarker("m16")
  val m17 = new CodeMarker("m17")
  val m18 = new CodeMarker("m18")
  val m19 = new CodeMarker("m19")
  val m20 = new CodeMarker("m20")

  implicit class CodeHelper(val sc: StringContext) extends AnyVal {

    /**
     * An interpolator that lets set marker inside a virtual source file.
     *
     * For instance:
     * ```
     * code"""object ${m1}Foo${m2} { def bar = ${m3}Hello{$m4}.quux }"""
     * ```
     *
     * This will define a source file where the markers `m1` and `m2` enclose the identifier `Foo`,
     * and `m3` and `m4` enclose the identifier `Hello`. These positions can then be used to ask to
     * perform actions such as finding all references, etc.
     */
    def code(args: Embedded*): ScalaSourceWithPositions = {
      val (text, positions) = textAndPositions(args: _*)
      ScalaSourceWithPositions(text, positions)
    }

    /**
     * An interpolator similar to `code`, but used for defining a worksheet.
     *
     * @see code
     */
    def ws(args: Embedded*): WorksheetWithPositions = {
      val (text, positions) = textAndPositions(args: _*)
      WorksheetWithPositions(text, positions)
    }

    /**
     * An interpolator similar to `code`, but used for defining a source that will
     * be unpickled from TASTY.
     *
     * @see code
     */
    def tasty(args: Embedded*): TastyWithPositions = {
      val (text, positions) = textAndPositions(args: _*)
      TastyWithPositions(text, positions)
    }

    private def textAndPositions(args: Embedded*): (String, List[(CodeMarker, Int, Int)]) = {
      val pi = sc.parts.iterator
      val ai = args.iterator

      var line = 0
      var char = 0
      def scan(str: String): Unit = {
        for (c <- str)
          if (c == '\n') { line += 1; char = 0 } else { char += 1 }
      }

      val stringBuilder = new StringBuilder
      val positions = List.newBuilder[(CodeMarker, Int, Int)]

      while (ai.hasNext) {
        val next = pi.next().stripMargin
        stringBuilder.append(next)
        scan(next)

        ai.next() match {
          case emb: CodeMarker =>
            positions += ((emb, line, char))

          case emb: CodeInRange =>
            positions += ((emb.range.start, line, char))
            scan(emb.text)
            stringBuilder.append(emb.text)
            positions += ((emb.range.end, line, char))
        }

      }

      if (pi.hasNext)
        stringBuilder.append(pi.next().stripMargin)

      (stringBuilder.result(), positions.result())
    }
  }

  /** A new `CodeTester` working with a single project containing `sources`. */
  def withSources(sources: SourceWithPositions*): CodeTester = withProjects(Project(sources.toList))

  /** A new `CodeTester` working with `projects`. */
  def withProjects(projects: Project*): CodeTester = new CodeTester(projects.toList)

  sealed trait SourceWithPositions {

    /** A name for this source given its index. */
    def sourceName(index: Int): String

    /** The code contained within the virtual source file. */
    def text: String

    /** The positions of the markers that have been set. */
    def positions: List[(CodeMarker, Int, Int)]

    /** A new `CodeTester` with only this source in the project. */
    def withSource: CodeTester = withSources(this)

  }

  object SourceWithPositions {
    import scala.language.implicitConversions

    implicit def sourceWithPositionsToCodeTester(scalaSource: SourceWithPositions): CodeTester =
      scalaSource.withSource
  }

  /**
   * A virtual Scala source file where several markers have been set.
   *
   * @param text      The code contained within the virtual source file.
   * @param positions The positions of the markers that have been set.
   */
  case class ScalaSourceWithPositions(text: String, positions: List[(CodeMarker, Int, Int)]) extends SourceWithPositions {
    def sourceName(index: Int): String = s"Source$index.scala"
  }

  /**
   * A virtual worksheet where several markers have been set.
   *
   * @param text      The code contained within the virtual source file.
   * @param positions The positions of the markers that have been set.
   */
  case class WorksheetWithPositions(text: String, positions: List[(CodeMarker, Int, Int)]) extends SourceWithPositions {
    def sourceName(index: Int): String = s"Worksheet$index.sc"
  }

  /**
   * A virtual source file that will not be opened in the IDE, but instead unpickled from TASTY.
   *
   * @param text      The code contained within the virtual source file.
   * @param positions The positions of the markers that have been set.
   */
  case class TastyWithPositions(text: String, positions: List[(CodeMarker, Int, Int)]) extends SourceWithPositions {
    def sourceName(index: Int): String = s"Source-from-tasty-$index.scala"
  }

  /**
   * A group of sources belonging to the same project.
   *
   * @param sources   The sources that this project holds.
   * @param name      The name of this project
   * @param dependsOn The other projects on which this project depend.
   */
  case class Project(sources: List[SourceWithPositions],
                     name: String = Project.freshName,
                     dependsOn: List[Project] = Nil) {

    /**
     * Add `sources` to the sources of this project.
     */
    def withSources(sources: SourceWithPositions*): Project = copy(sources = this.sources ::: sources.toList)

  }

  object Project {
    private[this] val count = new java.util.concurrent.atomic.AtomicInteger()
    private def freshName: String = s"project${count.incrementAndGet()}"

    /**
     * Creates a new project that depends on `projects`.
     *
     * @param projects The dependencies of the new project.
     * @return An empty project with a dependency on the specified projects.
     */
    def dependingOn(projects: Project*) = new Project(Nil, dependsOn = projects.toList)

    /**
     * Create a new project with the given sources.
     *
     * @param sources The sources to add to this project.
     * @return a new project containing the specified sources.
     */
    def withSources(sources: SourceWithPositions*): Project = new Project(sources.toList)
  }

}
