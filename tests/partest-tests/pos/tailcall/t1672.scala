// moved to pending.
/* Tail calls translates this program to:

final lazy object Test1672: Test1672$ = new Test1672$()
  final class Test1672$()  extends Object() { this: Test1672$.type =>
    @tailrec def bar: (x: Int)(y: Int)Nothing = {
      def tailLabel2: ($this: Test1672$.type)(x: Int)(y: Int)Nothing = {
        try {
          throw new scala.package.RuntimeException()
        } catch {
          def $anonfun: (x$1: Throwable)Nothing =
            x$1 match {
              case _: scala.package.Throwable =>
                tailLabel2($this)(x)(y)
            }
          closure($anonfun)
        }
      }
      tailLabel2(Test1672$.this)(x)(y)
    }
  }

Note the tail call to taillabel2 from the local method $anonfun.
LambdaLift doe snot know how to deal wioth this.
*/

object Test1672 {
  @annotation.tailrec
  def bar(x: Int)(y: Int) : Nothing = {
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar(x)(y)
    }
  }
}
