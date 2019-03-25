object Test extends App {

  case class Config() {
    println("config")
  }

  class Printer {
    def print() = println("printing")
    object cfg extends Config
    implied config for Config
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
  Copier.cfg
  Copier.config
  Copier.config2
}