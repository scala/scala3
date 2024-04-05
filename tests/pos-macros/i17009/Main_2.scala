def processLine(line: String): Unit = {
  Macro.transform {
    line.split(" ").nn
    ???
  }
}
