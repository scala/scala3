package dotty.tools.dotc.config

object Printers {

  class Printer {
    def println(msg: => String): Unit = System.out.println(msg)
  }

  object noPrinter extends Printer {
    override def println(msg: => String): Unit = ()
  }

  val default: Printer = new Printer
  val core: Printer = noPrinter
  val typr: Printer = noPrinter
  val constr: Printer = noPrinter
  val overload: Printer = noPrinter
  val implicits: Printer = noPrinter
  val subtyping: Printer = noPrinter
  val unapp: Printer = noPrinter
  val completions = noPrinter
  val gadts = noPrinter

}