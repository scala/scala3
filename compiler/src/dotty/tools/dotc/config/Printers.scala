package dotty.tools.dotc.config

object Printers {

  class Printer {
    def println(msg: => String): Unit = System.out.println(msg)
  }

  object noPrinter extends Printer {
    override def println(msg: => String): Unit = ()
  }

  val default: Printer = new Printer
  val dottydoc: Printer = noPrinter
  val core: Printer = noPrinter
  val typr: Printer = noPrinter
  val constr: Printer = noPrinter
  val checks: Printer = noPrinter
  val overload: Printer = noPrinter
  val implicits: Printer = noPrinter
  val implicitsDetailed: Printer = noPrinter
  val subtyping: Printer = noPrinter
  val unapp: Printer = noPrinter
  val gadts: Printer = noPrinter
  val hk: Printer = noPrinter
  val variances: Printer = noPrinter
  val incremental: Printer = noPrinter
  val config: Printer = noPrinter
  val transforms: Printer = noPrinter
  val completions: Printer = noPrinter
  val cyclicErrors: Printer = noPrinter
  val pickling: Printer = noPrinter
  val inlining: Printer = noPrinter
  val exhaustivity: Printer = noPrinter
  val patmatch: Printer = new Printer
  val simplify: Printer = noPrinter
}
