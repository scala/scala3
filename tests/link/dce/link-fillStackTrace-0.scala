import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    try {
      throw new ThrowableFoo
    } catch {
      case _: ThrowableFoo => System.out.println(42)
    }
  }
}

class ThrowableFoo extends Throwable {
  @internal.link.AssertReachable
  override def fillInStackTrace(): Throwable = this
}
