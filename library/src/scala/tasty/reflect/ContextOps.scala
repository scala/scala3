package scala.tasty
package reflect

trait ContextOps extends Core {

  given ContextOps: extension (self: Context) {
    /** Returns the owner of the context */
    def owner: Symbol = internal.Context_owner(self)

    /** Returns the source file being compiled. The path is relative to the current working directory. */
    def source: java.nio.file.Path = internal.Context_source(self)

    /** Get package symbol if package is either defined in current compilation run or present on classpath. */
    def requiredPackage(path: String): Symbol = internal.Context_requiredPackage(self)(path)

    /** Get class symbol if class is either defined in current compilation run or present on classpath. */
    def requiredClass(path: String): Symbol = internal.Context_requiredClass(self)(path)

    /** Get module symbol if module is either defined in current compilation run or present on classpath. */
    def requiredModule(path: String): Symbol = internal.Context_requiredModule(self)(path)

    /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
    def requiredMethod(path: String): Symbol = internal.Context_requiredMethod(self)(path)

  }

  /** Context of the macro expansion */
  implicit def rootContext: Context = internal.rootContext

}
