package dotty.tools.dotc.config

object Printers {

  class Printer {
    def println(msg: => String): Unit = System.out.println(msg)
    def echo[T](msg: => String, value: T): T = { println(msg + value); value }
  }

  object noPrinter extends Printer {
    override def println(msg: => String): Unit = ()
    override def echo[T](msg: => String, value: T): T = value
  }

  val default: Printer = new Printer
  val core: Printer = noPrinter
  val typr: Printer = noPrinter
  val constr: Printer = noPrinter
  val checks: Printer = noPrinter
  val overload: Printer = noPrinter
  val implicits: Printer = noPrinter
  val implicitsDetailed: Printer = noPrinter
  val subtyping: Printer = noPrinter
  val unapp: Printer = noPrinter
  val completions = noPrinter
  val gadts = noPrinter
  val hk = noPrinter
  val variances = noPrinter
  val incremental = noPrinter
  val config = noPrinter
  val transforms = noPrinter
  val cyclicErrors = noPrinter
  val pickling = noPrinter
}
