trait LogLevelType
object Test {
  type LogLevel = Int & LogLevelType
  final val ErrorLevel = 1.asInstanceOf[Int & LogLevelType]
  def main(args: Array[String]): Unit = {
    List(ErrorLevel, ErrorLevel)
  }
}
