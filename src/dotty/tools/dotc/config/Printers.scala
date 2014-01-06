package dotty.tools.dotc.config

object Printers {

  class Printer {
    def println(msg: => String): Unit = System.out.println(msg)
  }

  object noPrinter extends Printer {
    override def println(msg: => String): Unit = ()
  }

  val core: Printer = noPrinter
  val typr: Printer = noPrinter
  val constr: Printer = noPrinter
  val unapp: Printer = noPrinter

}