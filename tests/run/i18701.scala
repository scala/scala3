abstract class TA { def tb(): TB }
abstract class TB { def chk(): Unit }
class A:
  def a(): TA =
    new TA {
      def tb(): TB =
        val fn: () => TB = () => new TB {
          def chk() = println(getClass.getEnclosingMethod())
        }
        fn()
    }

object Test:
  def main(args: Array[String]): Unit = new A().a().tb().chk()
