object Test extends logadapter.Api.SelfLogging:
  def main(args: Array[String]): Unit =
    try summon[logadapter.LogAdapter].info("Hello")
    catch t => t.printStackTrace(System.out)
