//> using options -Xfatal-warnings -feature

// in BadPrinter.scala
import language.future
class BadPrinter extends Printer: // warn
  override def print(s: String): Unit = println("Bad!!!")
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
