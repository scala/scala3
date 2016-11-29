import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    try {
      throw new BreakControl
    } catch {
      case _: BreakControl => System.out.println(42)
    }
  }
}

class BreakControl extends NoStackTrace

trait NoStackTrace extends Throwable {
  override def fillInStackTrace(): Throwable = this
}
