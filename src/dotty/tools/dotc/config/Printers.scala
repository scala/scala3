package dotty.tools.dotc.config

object Printers {

  class Printer {
    def println(msg: => String): Unit = System.out.println(msg)
  }

  object noPrinter extends Printer {
    override def println(msg: => String): Unit = ()
  }

  val core: Printer = noPrinter
  val typr: Printer = new Printer
  val constr: Printer = new Printer
  val overload: Printer = noPrinter
  val implicits: Printer = noPrinter
  val unapp: Printer = noPrinter

}