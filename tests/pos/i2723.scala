trait App(init: given Array[String] => Unit) {
  inline def main(args: Array[String]): Unit = init given args
}
