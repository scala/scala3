package scala.tasty
package reflect

trait ContextOps extends Core {

  implicit class ContextAPI(self: Context) {
    /** Returns the owner of the context */
    def owner: Symbol = kernel.Context_owner(self)

    /** Returns the source file being compiled. The path is relative to the current working directory. */
    def source: java.nio.file.Path = kernel.Context_source(self)
  }

  /** Context of the macro expansion */
  implicit def rootContext: Context = kernel.rootContext

}
