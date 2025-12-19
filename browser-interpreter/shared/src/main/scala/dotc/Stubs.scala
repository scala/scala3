package dotc

import core._
import Types._
import Symbols._

/**
 * Stubs for unsupported features in the browser compiler.
 *
 * These features are disabled or provide no-op implementations
 * to allow compilation to proceed.
 */
object Stubs {

  /** Macro expansion is not supported in the browser compiler */
  object MacroExpansion {
    def expand(tree: ast.Trees.Tree)(using Contexts.Context): ast.Trees.Tree = {
      throw new UnsupportedOperationException("Macros are not supported in the browser compiler")
    }

    def isMacro(sym: Symbol): Boolean = false
  }

  /** Java interop is not supported */
  object JavaInterop {
    def loadJavaClass(name: String): Option[Symbol] = None

    def isJavaClass(sym: Symbol): Boolean = false

    def javaType(tp: Type): Type = tp
  }

  /** Classpath is simplified - no JAR/filesystem access */
  object Classpath {
    def lookup(name: String): Option[Symbol] = None

    def classes: List[Symbol] = Nil

    def packages: List[Symbol] = Nil
  }

  /** Incremental compilation is not supported */
  object IncrementalCompilation {
    def invalidate(files: List[String]): Unit = ()

    def needsRecompilation(file: String): Boolean = true
  }

  /** Parallel compilation is not supported */
  object ParallelCompilation {
    val parallelism: Int = 1

    def inParallel[A](tasks: List[() => A]): List[A] = tasks.map(_.apply())
  }

  /** SemanticDB generation is not supported */
  object SemanticDB {
    def generate(tree: ast.Trees.Tree)(using Contexts.Context): Unit = ()
  }

  /** REPL support is not included */
  object REPL {
    def isEnabled: Boolean = false
  }

  /** IDE support is not included */
  object IDE {
    def isEnabled: Boolean = false

    def completion(pos: util.SourcePosition): List[String] = Nil

    def hover(pos: util.SourcePosition): Option[String] = None
  }
}

