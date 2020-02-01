trait App(init: Array[String] ?=> Unit) {
  inline def main(args: Array[String]): Unit = init(using args)
}
