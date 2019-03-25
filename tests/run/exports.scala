object Test extends App {

  case class Config()

  class Printer {
    def print() = println("printing")
    implied config for Config()
  }

  class Scanner {
    def scan() = println("scanning")
  }
  object Scanner extends Scanner

  object Copier {
    val printer = new Printer
    export printer._
    export implied printer._
    export Scanner.{scan => scanIt, _}

    val config2 = the[Config]
  }

  Copier.print()
  Copier.scanIt()
  Copier.config
  Copier.config2
}