object Test {
  def main(args: Array[String]): Unit =
    while(true)
      Runtime
        .getRuntime()
        .exec(Array("java", "-cp", System.getProperty("java.class.path"), "Test"));
}
