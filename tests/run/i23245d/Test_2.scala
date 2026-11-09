object Test extends BaseTest {
  def main(args: Array[String]): Unit =
    new Fixture { service.doIt("test") }
}
