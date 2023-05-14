def main(args: Array[String]): Unit =
  println("Hello " + util.Properties.propOrNull("key"))
