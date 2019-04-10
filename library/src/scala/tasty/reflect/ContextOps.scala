package scala.tasty
package reflect

trait ContextOps extends Core {

  implicit class ContextAPI(self: Context) {
    /** Returns the owner of the context */
    def owner: Symbol = kernel.Context_owner(self)

    /** Returns the source file being compiled. The path is relative to the current working directory. */
    def source: java.nio.file.Path = kernel.Context_source(self)

    /** Returns true if the generated strings are allowed to use colors */
    def printColors: Boolean = kernel.Context_printColors(self)

    /** Returns a new context where printColors is true */
    def withColors: Context = kernel.Context_withColors(self)

    /** Returns a new context where printColors is false */
    def withoutColors: Context = kernel.Context_withoutColors(self)
  }

  /** Context of the macro expansion */
  implicit def rootContext: Context = kernel.rootContext

}
