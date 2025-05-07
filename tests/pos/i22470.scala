trait A
trait OuterClass
trait MidClass
trait InnerClass

object Obj:
  def outerDef(a: A) =
    new OuterClass {
      def midDef(): Unit = {
        new MidClass {
          val valdef = new InnerClass {
            def innerDef() =
              println(a)
          }
        }
      }
    }
