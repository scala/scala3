package bar

// nested classes are not subclasses of `foo.Task` on the JVM and need protected accessors
class Sub extends foo.Task:
  def direct: String = foo.Task.poll()
  def inherited: String = foo.JSub.poll()
  def lambda: String = List(1).map(_ => foo.Task.poll()).head
  def anon: String = (new java.util.function.Supplier[String] { def get() = foo.Task.poll() }).get()
  inline def viaInline: String = foo.Task.poll()
  def inner: String = Inner().get
  class Inner { def get: String = foo.Task.poll() }
  def field: Int =
    val r = new Runnable { def run() = foo.Task.counter = foo.Task.counter + 1 }
    r.run()
    foo.Task.counter
