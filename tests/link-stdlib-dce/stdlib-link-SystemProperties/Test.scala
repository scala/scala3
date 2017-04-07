import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println(scala.sys.SystemProperties.noTraceSupression.value)
  }
}
