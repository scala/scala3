import scala.quoted.*
import scala.tasty.inspector.*

/* Test that the constructor of a trait has the StableRealizable trait if and
 * only if the trait has NoInits. This is used by the TASTy reader in Scala 2
 * to determine whether the constructor should be kept or discarded, and
 * consequently whether calls to $init$ should be emitted for the trait or not.
 */

// Definitions to be inspected

trait I9970IOApp {
  protected val foo = 23

  def run(args: List[String]): Int

  final def main(args: Array[String]): Unit = {
    sys.exit(run(args.toList))
  }
}

object I9970IOApp {
  trait Simple extends I9970IOApp {
    def run: Unit

    final def run(args: List[String]): Int = {
      run
      0
    }
  }
}

// TASTy Inspector test boilerplate

object Test {
  def main(args: Array[String]): Unit = {
    // Artefact of the current test infrastructure
    // TODO improve infrastructure to avoid needing this code on each test
    val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
    val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
    val tastyFiles = allTastyFiles.filter(_.contains("I9970"))

    TastyInspector.inspectTastyFiles(tastyFiles)(new TestInspector())
  }
}

// Inspector that performs the actual tests

class TestInspector() extends Inspector:

  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    var foundIOApp: Boolean = false
    var foundSimple: Boolean = false

    def inspectClass(using Quotes)(tree: quotes.reflect.Tree): Unit =
      import quotes.reflect.*
      tree match
        case t: PackageClause =>
          t.stats.foreach(inspectClass(_))

        case t: ClassDef if t.name.endsWith("$") =>
          t.body.foreach(inspectClass(_))

        case t: ClassDef =>
          t.name match
            case "I9970IOApp" =>
              foundIOApp = true
              // Cannot test the following because NoInits is not part of the quotes API
              //assert(!t.symbol.flags.is(Flags.NoInits))
              assert(!t.constructor.symbol.flags.is(Flags.StableRealizable))

            case "Simple" =>
              foundSimple = true
              // Cannot test the following because NoInits is not part of the quotes API
              //assert(t.symbol.flags.is(Flags.NoInits))
              assert(t.constructor.symbol.flags.is(Flags.StableRealizable))

            case _ =>
              assert(false, s"unexpected ClassDef '${t.name}'")

        case _ =>

    for tasty <- tastys do
      foundIOApp = false
      foundSimple = false
      inspectClass(tasty.ast)
      // Sanity check to make sure that our pattern matches are not simply glossing over the things we want to test
      assert(foundIOApp, "the inspector did not encounter IOApp")
      assert(foundSimple, "the inspect did not encounter IOApp.Simple")

  end inspect
