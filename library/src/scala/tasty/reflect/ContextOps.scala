package scala.tasty
package reflect

trait ContextOps extends ReflectionCore {

  trait ContextAPI {
    def owner: Symbol

    /** Returns the source file being compiled. The path is relative to the current working directory. */
    def source: java.nio.file.Path
  }
  implicit def ContextDeco(ctx: Context): ContextAPI

  implicit def rootContext: Context

  /** Root position of this tasty context. For macros it corresponds to the expansion site. */
  def rootPosition: Position

}
