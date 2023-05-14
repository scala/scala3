// in BadPrinter.scala
import language.future
class BadPrinter extends Printer: // error
  override def print(s: String): Unit = println("Bad!!!")