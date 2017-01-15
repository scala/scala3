import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 36, classesWithReachableMethods = 12, reachableMethods = 64)
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
