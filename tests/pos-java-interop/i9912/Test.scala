class Test {
  val logger = new JavaLogger
  def log(): Unit = {
      logger.info(
        "My {} String {} with multiple args {}",
        Array("a", "b", "c")*
      )
  }
}
