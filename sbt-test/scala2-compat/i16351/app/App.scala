package app

import lib.*

object App {
  def main(args: Array[String]): Unit =
    new Lib(Value("Foo"), b = 2) {}
}
