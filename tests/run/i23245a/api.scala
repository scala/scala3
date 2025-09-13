
package logadapter:

  trait AbstractLogAdapter:
    def info(message: String): Unit

  trait AbstractApi[T <: AbstractLogAdapter]:
    def logAdapterFor(loggerName: String): T
    trait SelfLogging:
      given adapter: T = logAdapterFor(this.getClass.getName)
      // workaround:
      //given () => T = logAdapterFor(this.getClass.getName)
      // or
      //private val adapter = logAdapterFor(this.getClass.getName)
      //given T = adapter
      // or just pollute the interface so it's never taken as pure
      //private val z = 42

  object Api extends AbstractApi[LogAdapter]:
    def logAdapterFor(loggerName: String): LogAdapter = new LogAdapter(loggerName)

  class LogAdapter(loggerName: String) extends AbstractLogAdapter:
    def info(message: String): Unit = System.err.println(s"INFO [${loggerName}] ${message}")
