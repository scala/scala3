package scala.tasty.interpreter
package pure

import scala.quoted.*
import scala.tasty.inspector.*

/**
 * Entry point for running the pure TASTy interpreter via TASTy Inspector.
 */
class PureInterpreterInspector extends Inspector {

  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
    import quotes.reflect.*

    object MainFinder extends TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        case DefDef("main", _, _, Some(rhs)) if isMainMethod(tree.symbol) =>
          println(s"[PureInterpreter] Found main method, executing...")
          val interpreter = new PureTastyInterpreter

          try {
            interpreter.eval(rhs)(using Map.empty)
            println(s"[PureInterpreter] Execution completed successfully")
          } catch {
            case e: MatchError =>
              println(s"[PureInterpreter] MatchError: ${e.getMessage}")
              println(s"[PureInterpreter] This likely means a tree node is not yet supported")
              throw e
            case e: Exception =>
              println(s"[PureInterpreter] Error: ${e.getMessage}")
              throw e
          }

        case _: PackageClause | _: ClassDef =>
          super.traverseTree(tree)(owner)

        case _ =>
          // Don't recurse into other definitions
      }

      private def isMainMethod(sym: Symbol): Boolean = {
        sym.flags.is(Flags.Method) &&
        sym.owner.flags.is(Flags.Module)
      }
    }

    for tasty <- tastys do
      MainFinder.traverseTree(tasty.ast)(Symbol.spliceOwner)
  }
}

/**
 * Standalone runner for the pure interpreter.
 */
object PureInterpreterMain {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage: PureInterpreterMain <tasty-files...>")
      println("  Interprets the main method found in the given TASTy files")
      sys.exit(1)
    }

    val tastyFiles = args.toList
    println(s"[PureInterpreter] Loading ${tastyFiles.size} TASTy file(s)")

    TastyInspector.inspectTastyFiles(tastyFiles)(new PureInterpreterInspector)
  }

  /**
   * Convenience method for testing from within the project.
   */
  def interpretFiles(files: List[String]): String = {
    val output = new java.io.ByteArrayOutputStream()
    scala.Console.withOut(output) {
      TastyInspector.inspectTastyFiles(files)(new PureInterpreterInspector)
    }
    output.toString
  }
}

