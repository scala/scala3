/* In the following we should never have two nested closures after phase betaReduce
 * The output of the program should instead look like this:

      package <empty> {
        @scala.annotation.internal.SourceFile("i6375.scala") class Test() extends
          Object
        () {
          final given def given_Int: Int = 0
          @scala.annotation.internal.ContextResultCount(1) def f(): (Int) ?=> Boolean
            =
          {
            def $anonfun(using evidence$1: Int): Boolean = true
            closure($anonfun)
          }
          @scala.annotation.internal.ContextResultCount(1) inline def g():
            (Int) ?=> Boolean
          =
            {
              def $anonfun(using evidence$3: Int): Boolean = true
              closure($anonfun)
            }
          {
            {
              def $anonfun(using evidence$3: Int): Boolean = true
              closure($anonfun)
            }
          }.apply(this.given_Int)
        }
      }
 */
class Test:
  given Int = 0

  def f(): Int ?=> Boolean = true : (Int ?=> Boolean)

  inline def g(): Int ?=> Boolean = true
  g()

