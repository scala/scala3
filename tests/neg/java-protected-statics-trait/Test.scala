package bar

// trait code is not in a subclass of `foo.Task` on the JVM
trait T extends foo.Task:
  def viaSelect: String = foo.Task.poll() // error
  def viaIdent: String =
    import foo.Task.poll
    poll() // error
  def viaAnon: String = (new java.util.function.Supplier[String] { def get() = foo.Task.poll() }).get() // error
